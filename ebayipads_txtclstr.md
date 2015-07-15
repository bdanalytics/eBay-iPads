# eBay:iPads:: sold classification:: txtclstr
bdanalytics  

**  **    
**Date: (Wed) Jul 22, 2015**    

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
    All.X.no.rnorm.rf: Leaderboard: 0.79363 
        newobs_tbl=[N=537, Y=261]; submit_filename=txtclstr_Final_rf_submit
        OOB_conf_mtrx=[YN=104, NY=61]=165; max.Accuracy.OOB=0.8135593
            opt.prob.threshold.OOB=0.5
            startprice.log=100.00; biddable=79.99; idseq.my=64.94; 
                prdline.my=4.14; prdline.my.clusterid=1.15; 
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
glb_out_pfx <- "txtclstr_"
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
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
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

![](ebayipads_txtclstr_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor    bgn end elapsed
## 1 import.data          1          0 12.165  NA      NA
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

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 1  import.data          1          0 12.165 12.642   0.478
## 2 inspect.data          2          0 12.643     NA      NA
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

![](ebayipads_txtclstr_files/figure-html/inspect.data-1.png) 

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

![](ebayipads_txtclstr_files/figure-html/inspect.data-2.png) 

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

![](ebayipads_txtclstr_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: .rnorm"
```

![](ebayipads_txtclstr_files/figure-html/inspect.data-4.png) 

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
## 2 inspect.data          2          0 12.643 18.606   5.964
## 3   scrub.data          2          1 18.607     NA      NA
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
##   0          0 1595     0      0        0       0       0
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
##   0          0 1599     0      0        0       0       0
##   1        292    0     6     36       27     172     196
##   Unknown    0    0     0      0        0     331       0
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor    bgn   end elapsed
## 3     scrub.data          2          1 18.607 19.85   1.243
## 4 transform.data          2          2 19.850    NA      NA
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
for (row_pos in c(178, 948, 1904, 2157, 2210)) {
#     tmp_str <- unlist(strsplit(glb_allobs_df[row_pos, "descr.my"], ""))
#     glb_allobs_df[row_pos, "descr.my"] <- paste0(tmp_str[!tmp_str %in% hex_vctr],
#                                                          collapse="")
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
## 4   transform.data          2          2 19.850 20.371   0.521
## 5 extract.features          3          0 20.371     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor   bgn end elapsed
## 1 extract.features_bgn          1          0 20.38  NA      NA
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
## 1                extract.features_bgn          1          0 20.380 20.396
## 2 extract.features_factorize.str.vars          2          0 20.396     NA
##   elapsed
## 1   0.016
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##   description     condition      cellular       carrier         color 
## "description"   "condition"    "cellular"     "carrier"       "color" 
##       storage   productline          .src    prdline.my      descr.my 
##     "storage" "productline"        ".src"  "prdline.my"    "descr.my"
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
## 2 extract.features_factorize.str.vars          2          0 20.396 20.978
## 3       extract.features_process.text          3          0 20.979     NA
##   elapsed
## 2   0.583
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
## 3          0 20.979 22.779   1.801
## 4          1 22.780     NA      NA
## [1] "Remaining compound terms in descr.my: "
##                                                    label step_major
## 4 extract.features_process.text_reporting_compound_terms          3
## 5                          extract.features_build.corpus          4
##   step_minor    bgn    end elapsed
## 4          1 22.780 22.784   0.004
## 5          0 22.785     NA      NA
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
## [1] "Rows: 783; Cols: 4"
##              TfIdf      term freq pos
## condition 207.7265 condition  497 161
## new       125.4260       new  156 473
## used      122.9402      used  239 748
## good      120.7204      good  197 317
## scratches 113.7194 scratches  254 613
## screen    105.9815    screen  210 615
##               TfIdf       term freq pos
## turns      4.079877      turns    3 724
## battery    3.613332    battery    4  77
## appears    2.450047    appears    2  53
## exhibition 2.275334 exhibition    1 263
## single     2.190630     single    2 654
## past       1.264074       past    1 514
##            TfIdf   term freq pos
## high   1.1376668   high    1 342
## hole   1.1376668   hole    1 344
## lights 1.1376668 lights    1 406
## person 1.1376668 person    1 519
## places 1.1376668 places    1 535
## 79in   0.9480557   79in    1  16
##            TfIdf   term freq pos
## high   1.1376668   high    1 342
## hole   1.1376668   hole    1 344
## lights 1.1376668 lights    1 406
## person 1.1376668 person    1 519
## places 1.1376668 places    1 535
## 79in   0.9480557   79in    1  16
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
## [1] "Rows: 645; Cols: 4"
##            TfIdf    term freq pos
## condit  207.4213  condit  494 134
## use     146.0063     use  290 614
## scratch 127.9519 scratch  286 502
## new     125.4260     new  156 388
## good    120.8098    good  197 262
## screen  106.8461  screen  213 503
##              TfIdf     term freq pos
## damag    35.125499    damag   31 158
## come     34.442837     come   55 130
## tear     20.833717     tear   31 575
## keyboard  7.518084 keyboard    6 315
## otter     5.958097    otter    5 410
## known     1.896111    known    1 320
##           TfIdf  term freq pos
## final 1.1376668 final    1 230
## fold  1.1376668  fold    1 238
## goe   1.1376668   goe    1 260
## high  1.1376668  high    1 282
## hole  1.1376668  hole    1 284
## 79in  0.9480557  79in    1  16
##           TfIdf  term freq pos
## final 1.1376668 final    1 230
## fold  1.1376668  fold    1 238
## goe   1.1376668   goe    1 260
## high  1.1376668  high    1 282
## hole  1.1376668  hole    1 284
## 79in  0.9480557  79in    1  16
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
## terms.n.post.stop         -0.083229193
## TfIdf.sum.post.stop       -0.032916896
## terms.n.post.stem         -0.083078046
## TfIdf.sum.post.stem       -0.034803081
## terms.n.stem.stop.Ratio    0.016372740
## TfIdf.sum.stem.stop.Ratio  0.001151129
##                           label step_major step_minor    bgn   end elapsed
## 5 extract.features_build.corpus          4          0 22.785 34.21  11.425
## 6  extract.features_extract.DTM          5          0 34.211    NA      NA
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
## 6 extract.features_extract.DTM          5          0 34.211 35.968   1.757
## 7  extract.features_report.DTM          6          0 35.969     NA      NA
## [1] "Reporting TfIDf terms for descr.my..."
## [1] "   Full TermMatrix:"
## <<DocumentTermMatrix (documents: 2659, terms: 645)>>
## Non-/sparse entries: 8321/1706734
## Sparsity           : 100%
## Maximal term length: 16
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
## [1] "   Sparse TermMatrix:"
## <<DocumentTermMatrix (documents: 2659, terms: 8)>>
## Non-/sparse entries: 2065/19207
## Sparsity           : 90%
## Maximal term length: 7
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
```

```
## Warning in myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full",
## colorcol_name = "in.sprs"): converting in.sprs to class:factor
```

![](ebayipads_txtclstr_files/figure-html/extract.features-1.png) 

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](ebayipads_txtclstr_files/figure-html/extract.features-2.png) ![](ebayipads_txtclstr_files/figure-html/extract.features-3.png) 

```
## Warning in rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df,
## terms_TfIdf_df): object 'full_TfIdf_mtrx' not found
```

```
##                         label step_major step_minor    bgn    end elapsed
## 7 extract.features_report.DTM          6          0 35.969 38.099    2.13
## 8   extract.features_bind.DTM          7          0 38.099     NA      NA
## [1] "Binding DTM for descr.my..."
##                       label step_major step_minor    bgn    end elapsed
## 8 extract.features_bind.DTM          7          0 38.099 38.779    0.68
## 9 extract.features_bind.DXM          8          0 38.779     NA      NA
## [1] "Binding DXM for descr.my..."
```

```
## Warning in rm(log_X_df, txt_X_df): object 'log_X_df' not found
```

![](ebayipads_txtclstr_files/figure-html/extract.features-4.png) 

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

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                        label step_major step_minor    bgn   end elapsed
## 9  extract.features_bind.DXM          8          0 38.779 78.19  39.412
## 10      extract.features_end          9          0 78.191    NA      NA
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
## 9          0 38.779 78.190  39.412   39.411
## 5          0 22.785 34.210  11.425   11.425
## 7          0 35.969 38.099   2.130    2.130
## 3          0 20.979 22.779   1.801    1.800
## 6          0 34.211 35.968   1.757    1.757
## 8          0 38.099 38.779   0.680    0.680
## 2          0 20.396 20.978   0.583    0.582
## 1          0 20.380 20.396   0.016    0.016
## 4          1 22.780 22.784   0.004    0.004
## [1] "Total Elapsed Time: 78.19 secs"
```

![](ebayipads_txtclstr_files/figure-html/extract.features-5.png) 

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

![](ebayipads_txtclstr_files/figure-html/extract.features-6.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5 extract.features          3          0 20.371 79.628  59.257
## 6     cluster.data          4          0 79.629     NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 6        cluster.data          4          0 79.629 80.782   1.154
## 7 manage.missing.data          4          1 80.783     NA      NA
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
##                    1446                    1001                      31 
##           cellular.fctr     D.terms.n.post.stop   D.TfIdf.sum.post.stop 
##                    1599                    1522                    1522 
##     D.terms.n.post.stem   D.TfIdf.sum.post.stem              D.T.condit 
##                    1522                    1522                    2165 
##                 D.T.use             D.T.scratch                 D.T.new 
##                    2369                    2373                    2503 
##                D.T.good              D.T.screen                D.T.ipad 
##                    2462                    2446                    2427 
##               D.T.great                D.T.work               D.T.excel 
##                    2534                    2462                    2560 
##             D.nwrds.log         D.nwrds.unq.log             D.sum.TfIdf 
##                    1521                    1522                    1522 
## D.ratio.sum.TfIdf.nwrds             D.nchrs.log             D.nuppr.log 
##                    1522                    1521                    1523 
##             D.ndgts.log           D.npnct01.log           D.npnct02.log 
##                    2428                    2581                    2659 
##           D.npnct03.log           D.npnct04.log           D.npnct05.log 
##                    2616                    2659                    2594 
##           D.npnct06.log           D.npnct07.log           D.npnct08.log 
##                    2556                    2658                    2583 
##           D.npnct09.log           D.npnct10.log           D.npnct11.log 
##                    2643                    2650                    2303 
##           D.npnct12.log           D.npnct13.log           D.npnct14.log 
##                    2539                    1934                    2584 
##           D.npnct15.log           D.npnct16.log           D.npnct17.log 
##                    2638                    2548                    2659 
##           D.npnct18.log           D.npnct19.log           D.npnct20.log 
##                    2658                    2659                    2659 
##           D.npnct21.log           D.npnct22.log           D.npnct23.log 
##                    2659                    2659                    2659 
##           D.npnct24.log           D.npnct25.log           D.npnct26.log 
##                    1521                    2659                    2659 
##           D.npnct27.log           D.npnct28.log           D.npnct29.log 
##                    2659                    2651                    2659 
##           D.npnct30.log         D.nstopwrds.log                D.P.http 
##                    2659                    1664                    2659 
##                D.P.mini                 D.P.air 
##                    2625                    2639 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## description   condition    cellular     carrier       color     storage 
##        1521           0           0           0           0           0 
## productline  prdline.my    descr.my 
##           0           0        1521
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
##                    1446                    1001                      31 
##           cellular.fctr     D.terms.n.post.stop   D.TfIdf.sum.post.stop 
##                    1599                    1522                    1522 
##     D.terms.n.post.stem   D.TfIdf.sum.post.stem              D.T.condit 
##                    1522                    1522                    2165 
##                 D.T.use             D.T.scratch                 D.T.new 
##                    2369                    2373                    2503 
##                D.T.good              D.T.screen                D.T.ipad 
##                    2462                    2446                    2427 
##               D.T.great                D.T.work               D.T.excel 
##                    2534                    2462                    2560 
##             D.nwrds.log         D.nwrds.unq.log             D.sum.TfIdf 
##                    1521                    1522                    1522 
## D.ratio.sum.TfIdf.nwrds             D.nchrs.log             D.nuppr.log 
##                    1522                    1521                    1523 
##             D.ndgts.log           D.npnct01.log           D.npnct02.log 
##                    2428                    2581                    2659 
##           D.npnct03.log           D.npnct04.log           D.npnct05.log 
##                    2616                    2659                    2594 
##           D.npnct06.log           D.npnct07.log           D.npnct08.log 
##                    2556                    2658                    2583 
##           D.npnct09.log           D.npnct10.log           D.npnct11.log 
##                    2643                    2650                    2303 
##           D.npnct12.log           D.npnct13.log           D.npnct14.log 
##                    2539                    1934                    2584 
##           D.npnct15.log           D.npnct16.log           D.npnct17.log 
##                    2638                    2548                    2659 
##           D.npnct18.log           D.npnct19.log           D.npnct20.log 
##                    2658                    2659                    2659 
##           D.npnct21.log           D.npnct22.log           D.npnct23.log 
##                    2659                    2659                    2659 
##           D.npnct24.log           D.npnct25.log           D.npnct26.log 
##                    1521                    2659                    2659 
##           D.npnct27.log           D.npnct28.log           D.npnct29.log 
##                    2659                    2651                    2659 
##           D.npnct30.log         D.nstopwrds.log                D.P.http 
##                    2659                    1664                    2659 
##                D.P.mini                 D.P.air 
##                    2625                    2639 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## description   condition    cellular     carrier       color     storage 
##        1521           0           0           0           0           0 
## productline  prdline.my    descr.my 
##           0           0        1521
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
## [1] "glb_allobs_df$prdline.my Entropy: 0.6851 (99.2575 pct)"
##    prdline.my   N   Y  .entropy .knt
## 1     Unknown 118  80 0.6746159  198
## 2      iPad 1 102 125 0.6880053  227
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
## 130          0       0           0 0.8182532        0          0        0
##     D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 5           0        0         0        0        0       0
## 130         0        0         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 199     10199         N    Unknown
## 1458    11458         N    Unknown
##                                                    descr.my D.T.condit
## 199  In great working condition. Very little where in tear.  0.4856602
## 1458           Tested Working - Great condition! -  Grade A  0.4856602
##      D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad D.T.great
## 199        0           0       0        0          0        0 0.8821768
## 1458       0           0       0        0          0        0 0.8821768
##       D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 199  0.7509232         0        0        0       0
## 1458 0.7509232         0        0        0       0
```

![](ebayipads_txtclstr_files/figure-html/cluster.data-1.png) 

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
## 13  0.2207546       0           0       0 0.3413287          0        0
##    D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 9          0 0.0000000         0        0        0       0
## 13         0 0.3413287         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 13      10013         Y     iPad 1
## 1240    11240         N     iPad 1
##                                                                                                 descr.my
## 13      GOOD CONDITION. CLEAN ICLOUD. NO LOCKS. CLEAN IMEI. This tablet has been fully tested and works 
## 1240 Professionally inspected, reset and cleaned in very good cosmetic and working condition. Units may 
##      D.T.condit D.T.use D.T.scratch D.T.new  D.T.good D.T.screen D.T.ipad
## 13    0.2207546       0           0       0 0.3413287          0        0
## 1240  0.2428301       0           0       0 0.3754616          0        0
##      D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 13           0 0.3413287         0        0        0       0
## 1240         0 0.3754616         0        0        0       0
```

![](ebayipads_txtclstr_files/figure-html/cluster.data-2.png) 

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
## 1  0.8094336 0.0000000   0.0000000       0        0  0.0000000 1.172896
## 2  0.0000000 0.5812289   0.2924361       0        0  0.3310871 0.000000
##   D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 1         0        0         0        0        0       0
## 2         0        0         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 2     10002         Y     iPad 2
## 62    10062         Y     iPad 2
##                                                                                                descr.my
## 2  Previously used, please read description. May show signs of use such as scratches to the screen and 
## 62 Previously used, please read description. May show signs of use such as scratches to the screen and 
##    D.T.condit   D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 2           0 0.5812289   0.2924361       0        0  0.3310871        0
## 62          0 0.5812289   0.2924361       0        0  0.3310871        0
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 2          0        0         0        0        0       0
## 62         0        0         0        0        0       0
```

![](ebayipads_txtclstr_files/figure-html/cluster.data-3.png) 

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
## 11  0.2207546       0   0.2924361       0 0.3413287  0.3310871        0
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 3  0.0000000        0         0        0        0       0
## 11 0.4009894        0         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 31    10031         Y    iPad 3+
## 48    10048         Y    iPad 3+
##                                                                                            descr.my
## 31 Ipad has been previously used and shows some light signs of use.  Some light scratches on front 
## 48 Ipad has been previously used and shows some light signs of use.  Some light scratches on front 
##    D.T.condit   D.T.use D.T.scratch D.T.new D.T.good D.T.screen  D.T.ipad
## 31          0 0.6393518   0.3216797       0        0          0 0.3518687
## 48          0 0.6393518   0.3216797       0        0          0 0.3518687
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 31         0        0         0        0        0       0
## 48         0        0         0        0        0       0
```

![](ebayipads_txtclstr_files/figure-html/cluster.data-4.png) 

```
## [1] "Category: iPadAir"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 16    10016         N    iPadAir
## 33    10033         N    iPadAir
##                                                                                                descr.my
## 16                                                                                                     
## 33 We are selling good quality iPads that have been fully tested by an Apple Certified Technician. The 
##    D.T.condit D.T.use D.T.scratch D.T.new  D.T.good D.T.screen  D.T.ipad
## 16          0       0           0       0 0.0000000          0 0.0000000
## 33          0       0           0       0 0.4171796          0 0.3909652
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
## 44    0.2207546       0           0       0        0          0        0
## 1233  1.2141504       0           0       0        0          0        0
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 44           0        0         0        0        0       0
## 1233         0        0         0        0        0       0
```

![](ebayipads_txtclstr_files/figure-html/cluster.data-5.png) 

```
## [1] "Category: iPadmini"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 7     10007         Y   iPadmini
## 76    10076         Y   iPadmini
##                                                                                         descr.my
## 7                                                                                               
## 76 Works perfectly, NOT iCloud locked, 1 owner. It is in not in very good  condition, but works 
##    D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 7   0.0000000       0           0       0 0.000000          0        0
## 76  0.3035376       0           0       0 0.469327          0        0
##    D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 7          0 0.0000000         0        0        0       0
## 76         0 0.9386541         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 76    10076         Y   iPadmini
## 86    10086         Y   iPadmini
##                                                                                         descr.my
## 76 Works perfectly, NOT iCloud locked, 1 owner. It is in not in very good  condition, but works 
## 86 Works perfectly, NOT iCloud locked, 1 owner. It is in not in very good  condition, but works 
##    D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 76  0.3035376       0           0       0 0.469327          0        0
## 86  0.3035376       0           0       0 0.469327          0        0
##    D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 76         0 0.9386541         0        0        0       0
## 86         0 0.9386541         0        0        0       0
```

![](ebayipads_txtclstr_files/figure-html/cluster.data-6.png) 

```
## [1] "Category: iPadmini 2+"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr  prdline.my
## 4     10004         N iPadmini 2+
## 18    10018         N iPadmini 2+
##                                                                                                descr.my
## 4                                                                                                      
## 18 We are selling good quality iPads that have been fully tested by an Apple Certified Technician. The 
##    D.T.condit D.T.use D.T.scratch D.T.new  D.T.good D.T.screen  D.T.ipad
## 4           0       0           0       0 0.0000000          0 0.0000000
## 18          0       0           0       0 0.4171796          0 0.3909652
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 4          0        0         0        0        0       0
## 18         0        0         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##     UniqueID sold.fctr  prdline.my
## 668    10668         N iPadmini 2+
## 886    10886         N iPadmini 2+
##                                                                                                descr.my
## 668                                                        New Open Box, see pics and description below
## 886 New in the opened package.  All is still sealed.  Few small cuts into the outer box on the outside 
##     D.T.condit D.T.use D.T.scratch   D.T.new D.T.good D.T.screen D.T.ipad
## 668          0       0           0 0.6818776        0          0        0
## 886          0       0           0 0.4545851        0          0        0
##     D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 668         0        0         0        0        0       0
## 886         0        0         0        0        0       0
```

![](ebayipads_txtclstr_files/figure-html/cluster.data-7.png) 

```
## [1] "glb_allobs_df$prdline.my$.clusterid Entropy: 0.6735 (98.3068 pct)"
##    prdline.my.clusterid   N   Y  .entropy .knt
## 1             Unknown_1  77  53 0.6760076  130
## 2             Unknown_2  26  22 0.6896709   48
## 3             Unknown_3  15   5 0.5623351   20
## 4              iPad 1_1  64  88 0.6806295  152
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
## 24           iPadmini_2  18  15 0.6890092   33
## 25           iPadmini_3  15  19 0.6862107   34
## 26           iPadmini_4  14  12 0.6901857   26
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
## 7 manage.missing.data          4          1 80.783 84.236   3.453
## 8     select.features          5          0 84.236     NA      NA
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
## biddable                                       biddable  0.5485860292
## startprice.log                           startprice.log -0.4668597069
## startprice                                   startprice -0.4556701005
## UniqueID                                       UniqueID -0.1904241613
## idseq.my                                       idseq.my -0.1904241613
## condition.fctr                           condition.fctr -0.1531007887
## D.npnct05.log                             D.npnct05.log -0.1178427734
## D.terms.n.post.stop                 D.terms.n.post.stop -0.0832291933
## D.terms.n.post.stem                 D.terms.n.post.stem -0.0830780464
## D.npnct14.log                             D.npnct14.log -0.0784119354
## cellular.fctr                             cellular.fctr -0.0722383245
## carrier.fctr                               carrier.fctr -0.0677807650
## D.nwrds.unq.log                         D.nwrds.unq.log -0.0652201034
## D.ndgts.log                                 D.ndgts.log -0.0633901377
## D.npnct09.log                             D.npnct09.log -0.0617301891
## D.nwrds.log                                 D.nwrds.log -0.0592009676
## D.nchrs.log                                 D.nchrs.log -0.0566417632
## D.npnct12.log                             D.npnct12.log -0.0565901515
## D.ratio.nstopwrds.nwrds         D.ratio.nstopwrds.nwrds  0.0555882334
## D.nuppr.log                                 D.nuppr.log -0.0554847949
## D.npnct28.log                             D.npnct28.log -0.0523776347
## D.npnct06.log                             D.npnct06.log -0.0497583609
## D.nstopwrds.log                         D.nstopwrds.log -0.0467502550
## D.npnct24.log                             D.npnct24.log -0.0459503420
## D.npnct16.log                             D.npnct16.log -0.0447153098
## color.fctr                                   color.fctr -0.0426291775
## prdline.my.fctr                         prdline.my.fctr -0.0403911061
## D.npnct15.log                             D.npnct15.log  0.0400921803
## D.npnct08.log                             D.npnct08.log -0.0394718187
## D.T.condit                                   D.T.condit -0.0383672486
## D.T.new                                         D.T.new -0.0381309885
## D.npnct13.log                             D.npnct13.log -0.0367501751
## D.TfIdf.sum.post.stem             D.TfIdf.sum.post.stem -0.0348030805
## D.sum.TfIdf                                 D.sum.TfIdf -0.0348030805
## D.TfIdf.sum.post.stop             D.TfIdf.sum.post.stop -0.0329168959
## .clusterid                                   .clusterid -0.0327410909
## .clusterid.fctr                         .clusterid.fctr -0.0327410909
## D.T.excel                                     D.T.excel  0.0266953752
## D.npnct03.log                             D.npnct03.log  0.0258721719
## D.npnct07.log                             D.npnct07.log  0.0250156240
## D.T.screen                                   D.T.screen  0.0244639185
## D.npnct10.log                             D.npnct10.log -0.0240301079
## .rnorm                                           .rnorm -0.0228561937
## D.npnct18.log                             D.npnct18.log -0.0214919447
## D.npnct11.log                             D.npnct11.log -0.0188179512
## D.terms.n.stem.stop.Ratio     D.terms.n.stem.stop.Ratio  0.0163727398
## D.T.ipad                                       D.T.ipad -0.0161124901
## D.T.work                                       D.T.work -0.0138627742
## D.T.use                                         D.T.use  0.0115847047
## D.P.mini                                       D.P.mini -0.0111321924
## storage.fctr                               storage.fctr -0.0103459049
## D.P.air                                         D.P.air -0.0091681483
## D.ratio.sum.TfIdf.nwrds         D.ratio.sum.TfIdf.nwrds  0.0083890176
## D.T.great                                     D.T.great  0.0080288313
## D.T.scratch                                 D.T.scratch -0.0068979379
## D.npnct01.log                             D.npnct01.log  0.0042941114
## D.TfIdf.sum.stem.stop.Ratio D.TfIdf.sum.stem.stop.Ratio  0.0011511285
## D.T.good                                       D.T.good -0.0003015249
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
## biddable                                  0 0.5485860292
## startprice.log                            0 0.4668597069
## startprice                                1 0.4556701005
## UniqueID                                  1 0.1904241613
## idseq.my                                  0 0.1904241613
## condition.fctr                            0 0.1531007887
## D.npnct05.log                             0 0.1178427734
## D.terms.n.post.stop                       0 0.0832291933
## D.terms.n.post.stem                       0 0.0830780464
## D.npnct14.log                             0 0.0784119354
## cellular.fctr                             0 0.0722383245
## carrier.fctr                              0 0.0677807650
## D.nwrds.unq.log                           0 0.0652201034
## D.ndgts.log                               0 0.0633901377
## D.npnct09.log                             0 0.0617301891
## D.nwrds.log                               0 0.0592009676
## D.nchrs.log                               0 0.0566417632
## D.npnct12.log                             0 0.0565901515
## D.ratio.nstopwrds.nwrds                   0 0.0555882334
## D.nuppr.log                               0 0.0554847949
## D.npnct28.log                             0 0.0523776347
## D.npnct06.log                             0 0.0497583609
## D.nstopwrds.log                           0 0.0467502550
## D.npnct24.log                             0 0.0459503420
## D.npnct16.log                             0 0.0447153098
## color.fctr                                0 0.0426291775
## prdline.my.fctr                           0 0.0403911061
## D.npnct15.log                             0 0.0400921803
## D.npnct08.log                             0 0.0394718187
## D.T.condit                                1 0.0383672486
## D.T.new                                   1 0.0381309885
## D.npnct13.log                             0 0.0367501751
## D.TfIdf.sum.post.stem                     0 0.0348030805
## D.sum.TfIdf                               0 0.0348030805
## D.TfIdf.sum.post.stop                     0 0.0329168959
## .clusterid                                1 0.0327410909
## .clusterid.fctr                           0 0.0327410909
## D.T.excel                                 1 0.0266953752
## D.npnct03.log                             0 0.0258721719
## D.npnct07.log                             0 0.0250156240
## D.T.screen                                1 0.0244639185
## D.npnct10.log                             0 0.0240301079
## .rnorm                                    0 0.0228561937
## D.npnct18.log                             0 0.0214919447
## D.npnct11.log                             0 0.0188179512
## D.terms.n.stem.stop.Ratio                 0 0.0163727398
## D.T.ipad                                  1 0.0161124901
## D.T.work                                  1 0.0138627742
## D.T.use                                   1 0.0115847047
## D.P.mini                                  1 0.0111321924
## storage.fctr                              0 0.0103459049
## D.P.air                                   1 0.0091681483
## D.ratio.sum.TfIdf.nwrds                   0 0.0083890176
## D.T.great                                 1 0.0080288313
## D.T.scratch                               1 0.0068979379
## D.npnct01.log                             0 0.0042941114
## D.TfIdf.sum.stem.stop.Ratio               0 0.0011511285
## D.T.good                                  1 0.0003015249
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
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0348"
## [1] "cor(sold.fctr, D.sum.TfIdf)=-0.0348"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.sum.TfIdf as highly correlated with
## D.TfIdf.sum.post.stem
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
## [1] "cor(D.TfIdf.sum.post.stem, D.TfIdf.sum.post.stop)=0.9974"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0348"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stop)=-0.0329"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stop as highly correlated with
## D.TfIdf.sum.post.stem
```

```
## [1] "cor(D.nchrs.log, D.nwrds.unq.log)=0.9934"
## [1] "cor(sold.fctr, D.nchrs.log)=-0.0566"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0652"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nchrs.log as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(D.nwrds.log, D.nwrds.unq.log)=0.9927"
## [1] "cor(sold.fctr, D.nwrds.log)=-0.0592"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0652"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.log as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(D.nwrds.unq.log, D.terms.n.post.stop)=0.9755"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0652"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0832"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.unq.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.npnct24.log, D.ratio.nstopwrds.nwrds)=-0.9649"
## [1] "cor(sold.fctr, D.npnct24.log)=-0.0460"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0556"
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
## [1] "cor(D.TfIdf.sum.post.stem, D.ratio.nstopwrds.nwrds)=-0.9260"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0348"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0556"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stem as highly correlated with
## D.ratio.nstopwrds.nwrds
```

```
## [1] "cor(D.nstopwrds.log, D.terms.n.post.stop)=0.8952"
## [1] "cor(sold.fctr, D.nstopwrds.log)=-0.0468"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0832"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nstopwrds.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.ratio.nstopwrds.nwrds, D.terms.n.post.stop)=-0.8683"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0556"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0832"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.ratio.nstopwrds.nwrds as highly correlated
## with D.terms.n.post.stop
```

```
## [1] "cor(carrier.fctr, cellular.fctr)=0.8462"
## [1] "cor(sold.fctr, carrier.fctr)=-0.0678"
## [1] "cor(sold.fctr, cellular.fctr)=-0.0722"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified carrier.fctr as highly correlated with
## cellular.fctr
```

```
## [1] "cor(D.npnct13.log, D.terms.n.post.stop)=0.7383"
## [1] "cor(sold.fctr, D.npnct13.log)=-0.0368"
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
## 63                    biddable  0.5485860292               0 0.5485860292
## 56     D.ratio.nstopwrds.nwrds  0.0555882334               0 0.0555882334
## 36               D.npnct15.log  0.0400921803               0 0.0400921803
## 8                    D.T.excel  0.0266953752               1 0.0266953752
## 24               D.npnct03.log  0.0258721719               0 0.0258721719
## 28               D.npnct07.log  0.0250156240               0 0.0250156240
## 14                  D.T.screen  0.0244639185               1 0.0244639185
## 61   D.terms.n.stem.stop.Ratio  0.0163727398               0 0.0163727398
## 15                     D.T.use  0.0115847047               1 0.0115847047
## 57     D.ratio.sum.TfIdf.nwrds  0.0083890176               0 0.0083890176
## 10                   D.T.great  0.0080288313               1 0.0080288313
## 22               D.npnct01.log  0.0042941114               0 0.0042941114
## 19 D.TfIdf.sum.stem.stop.Ratio  0.0011511285               0 0.0011511285
## 9                     D.T.good -0.0003015249               1 0.0003015249
## 13                 D.T.scratch -0.0068979379               1 0.0068979379
## 4                      D.P.air -0.0091681483               1 0.0091681483
## 73                storage.fctr -0.0103459049               0 0.0103459049
## 6                     D.P.mini -0.0111321924               1 0.0111321924
## 16                    D.T.work -0.0138627742               1 0.0138627742
## 11                    D.T.ipad -0.0161124901               1 0.0161124901
## 32               D.npnct11.log -0.0188179512               0 0.0188179512
## 39               D.npnct18.log -0.0214919447               0 0.0214919447
## 3                       .rnorm -0.0228561937               0 0.0228561937
## 31               D.npnct10.log -0.0240301079               0 0.0240301079
## 1                   .clusterid -0.0327410909               1 0.0327410909
## 2              .clusterid.fctr -0.0327410909               0 0.0327410909
## 18       D.TfIdf.sum.post.stop -0.0329168959               0 0.0329168959
## 17       D.TfIdf.sum.post.stem -0.0348030805               0 0.0348030805
## 58                 D.sum.TfIdf -0.0348030805               0 0.0348030805
## 34               D.npnct13.log -0.0367501751               0 0.0367501751
## 12                     D.T.new -0.0381309885               1 0.0381309885
## 7                   D.T.condit -0.0383672486               1 0.0383672486
## 29               D.npnct08.log -0.0394718187               0 0.0394718187
## 69             prdline.my.fctr -0.0403911061               0 0.0403911061
## 66                  color.fctr -0.0426291775               0 0.0426291775
## 37               D.npnct16.log -0.0447153098               0 0.0447153098
## 45               D.npnct24.log -0.0459503420               0 0.0459503420
## 52             D.nstopwrds.log -0.0467502550               0 0.0467502550
## 27               D.npnct06.log -0.0497583609               0 0.0497583609
## 49               D.npnct28.log -0.0523776347               0 0.0523776347
## 53                 D.nuppr.log -0.0554847949               0 0.0554847949
## 33               D.npnct12.log -0.0565901515               0 0.0565901515
## 20                 D.nchrs.log -0.0566417632               0 0.0566417632
## 54                 D.nwrds.log -0.0592009676               0 0.0592009676
## 30               D.npnct09.log -0.0617301891               0 0.0617301891
## 21                 D.ndgts.log -0.0633901377               0 0.0633901377
## 55             D.nwrds.unq.log -0.0652201034               0 0.0652201034
## 64                carrier.fctr -0.0677807650               0 0.0677807650
## 65               cellular.fctr -0.0722383245               0 0.0722383245
## 35               D.npnct14.log -0.0784119354               0 0.0784119354
## 59         D.terms.n.post.stem -0.0830780464               0 0.0830780464
## 60         D.terms.n.post.stop -0.0832291933               0 0.0832291933
## 26               D.npnct05.log -0.1178427734               0 0.1178427734
## 67              condition.fctr -0.1531007887               0 0.1531007887
## 62                    UniqueID -0.1904241613               1 0.1904241613
## 68                    idseq.my -0.1904241613               0 0.1904241613
## 71                  startprice -0.4556701005               1 0.4556701005
## 72              startprice.log -0.4668597069               0 0.4668597069
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
## 70                    <NA>    1.163953    0.10746910   FALSE FALSE
## 63                    <NA>    1.223417    0.10746910   FALSE FALSE
## 56     D.terms.n.post.stop   13.222222    4.19129500   FALSE FALSE
## 36                    <NA>  153.500000    0.16120365   FALSE  TRUE
## 8                     <NA>  138.307692    0.75228372   FALSE  TRUE
## 24                    <NA>   83.318182    0.16120365   FALSE  TRUE
## 28                    <NA> 1860.000000    0.10746910   FALSE  TRUE
## 14                    <NA>   55.129032    0.80601827   FALSE  TRUE
## 61                    <NA>   77.913043    0.48361096   FALSE  TRUE
## 15                    <NA>   47.285714    0.91348737   FALSE  TRUE
## 57                    <NA>   63.058824   34.81998925   FALSE FALSE
## 10                    <NA>  104.588235    0.80601827   FALSE  TRUE
## 22                    <NA>   53.029412    0.32240731   FALSE  TRUE
## 19                    <NA>   65.235294   32.93927996   FALSE FALSE
## 9                     <NA>   47.888889    0.85975282   FALSE  TRUE
## 13                    <NA>   40.439024    0.85975282   FALSE  TRUE
## 4                     <NA>  123.000000    0.16120365   FALSE  TRUE
## 73                    <NA>    2.739003    0.26867276   FALSE FALSE
## 6                     <NA>   92.000000    0.16120365   FALSE  TRUE
## 16                    <NA>   63.740741    0.69854917   FALSE  TRUE
## 11                    <NA>   49.882353    0.80601827   FALSE  TRUE
## 32                    <NA>    9.385965    0.37614186   FALSE FALSE
## 39                    <NA> 1860.000000    0.10746910   FALSE  TRUE
## 3                     <NA>    1.000000  100.00000000   FALSE FALSE
## 31                    <NA>  309.000000    0.16120365   FALSE  TRUE
## 1                     <NA>    3.434659    0.26867276   FALSE FALSE
## 2                     <NA>    3.434659    0.26867276   FALSE FALSE
## 18   D.TfIdf.sum.post.stem   63.058824   34.39011284   FALSE FALSE
## 17 D.ratio.nstopwrds.nwrds   63.058824   34.28264374   FALSE FALSE
## 58   D.TfIdf.sum.post.stem   63.058824   34.28264374   FALSE FALSE
## 34     D.terms.n.post.stop    5.210728    0.32240731   FALSE FALSE
## 12                    <NA>  103.117647    0.85975282   FALSE  TRUE
## 7                     <NA>   24.918033    0.91348737   FALSE  TRUE
## 29                    <NA>   69.653846    0.21493821   FALSE  TRUE
## 69                    <NA>    1.135048    0.37614186   FALSE FALSE
## 66                    <NA>    1.576837    0.26867276   FALSE FALSE
## 37           D.npnct06.log   31.280702    0.16120365   FALSE  TRUE
## 45 D.ratio.nstopwrds.nwrds    1.355696    0.10746910   FALSE FALSE
## 52     D.terms.n.post.stop   13.448276    0.80601827   FALSE FALSE
## 27                    <NA>   33.773585    0.16120365   FALSE  TRUE
## 49                    <NA>  463.750000    0.16120365   FALSE  TRUE
## 53             D.nchrs.log   18.824561    4.40623321   FALSE FALSE
## 33                    <NA>   26.848485    0.21493821   FALSE  TRUE
## 20         D.nwrds.unq.log   15.985075    5.69586244   FALSE FALSE
## 54         D.nwrds.unq.log   12.903614    1.28962923   FALSE FALSE
## 30                    <NA>  308.666667    0.21493821   FALSE  TRUE
## 21                    <NA>   27.063492    0.69854917   FALSE  TRUE
## 55     D.terms.n.post.stop    7.657143    0.80601827   FALSE FALSE
## 64           cellular.fctr    3.192529    0.37614186   FALSE FALSE
## 65                    <NA>    2.120229    0.16120365   FALSE FALSE
## 35                    <NA>   35.372549    0.26867276   FALSE  TRUE
## 59     D.terms.n.post.stop    7.657143    0.80601827   FALSE FALSE
## 60                    <NA>    8.183206    0.80601827   FALSE FALSE
## 26                    <NA>   40.355556    0.10746910   FALSE  TRUE
## 67                    <NA>    4.006920    0.32240731   FALSE FALSE
## 62                    <NA>    1.000000  100.00000000   FALSE FALSE
## 68                    <NA>    1.000000  100.00000000   FALSE FALSE
## 71                    <NA>    2.807692   30.14508329   FALSE FALSE
## 72                    <NA>    2.807692   30.14508329   FALSE FALSE
## 5                     <NA>    0.000000    0.05373455    TRUE  TRUE
## 23                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 25                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 38                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 40                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 41                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 42                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 43                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 44                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 46                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 47                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 48                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 50                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 51                    <NA>    0.000000    0.05373455    TRUE  TRUE
##    myNearZV is.cor.y.abs.low
## 70    FALSE            FALSE
## 63    FALSE            FALSE
## 56    FALSE            FALSE
## 36    FALSE            FALSE
## 8     FALSE            FALSE
## 24    FALSE            FALSE
## 28     TRUE            FALSE
## 14    FALSE            FALSE
## 61    FALSE             TRUE
## 15    FALSE             TRUE
## 57    FALSE             TRUE
## 10    FALSE             TRUE
## 22    FALSE             TRUE
## 19    FALSE             TRUE
## 9     FALSE             TRUE
## 13    FALSE             TRUE
## 4     FALSE             TRUE
## 73    FALSE             TRUE
## 6     FALSE             TRUE
## 16    FALSE             TRUE
## 11    FALSE             TRUE
## 32    FALSE             TRUE
## 39     TRUE             TRUE
## 3     FALSE            FALSE
## 31    FALSE            FALSE
## 1     FALSE            FALSE
## 2     FALSE            FALSE
## 18    FALSE            FALSE
## 17    FALSE            FALSE
## 58    FALSE            FALSE
## 34    FALSE            FALSE
## 12    FALSE            FALSE
## 7     FALSE            FALSE
## 29    FALSE            FALSE
## 69    FALSE            FALSE
## 66    FALSE            FALSE
## 37    FALSE            FALSE
## 45    FALSE            FALSE
## 52    FALSE            FALSE
## 27    FALSE            FALSE
## 49    FALSE            FALSE
## 53    FALSE            FALSE
## 33    FALSE            FALSE
## 20    FALSE            FALSE
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

![](ebayipads_txtclstr_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##               id       cor.y exclude.as.feat  cor.y.abs cor.high.X
## 28 D.npnct07.log  0.02501562               0 0.02501562       <NA>
## 39 D.npnct18.log -0.02149194               0 0.02149194       <NA>
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
## 28      1860    0.10746910   FALSE TRUE     TRUE            FALSE
## 39      1860    0.10746910   FALSE TRUE     TRUE             TRUE
## 5          0    0.05373455    TRUE TRUE     TRUE               NA
## 23         0    0.05373455    TRUE TRUE     TRUE               NA
## 25         0    0.05373455    TRUE TRUE     TRUE               NA
## 38         0    0.05373455    TRUE TRUE     TRUE               NA
## 40         0    0.05373455    TRUE TRUE     TRUE               NA
## 41         0    0.05373455    TRUE TRUE     TRUE               NA
## 42         0    0.05373455    TRUE TRUE     TRUE               NA
## 43         0    0.05373455    TRUE TRUE     TRUE               NA
## 44         0    0.05373455    TRUE TRUE     TRUE               NA
## 46         0    0.05373455    TRUE TRUE     TRUE               NA
## 47         0    0.05373455    TRUE TRUE     TRUE               NA
## 48         0    0.05373455    TRUE TRUE     TRUE               NA
## 50         0    0.05373455    TRUE TRUE     TRUE               NA
## 51         0    0.05373455    TRUE TRUE     TRUE               NA
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
##                    1446                    1001                      31 
##           cellular.fctr     D.terms.n.post.stop   D.TfIdf.sum.post.stop 
##                    1599                    1522                    1522 
##     D.terms.n.post.stem   D.TfIdf.sum.post.stem              D.T.condit 
##                    1522                    1522                    2165 
##                 D.T.use             D.T.scratch                 D.T.new 
##                    2369                    2373                    2503 
##                D.T.good              D.T.screen                D.T.ipad 
##                    2462                    2446                    2427 
##               D.T.great                D.T.work               D.T.excel 
##                    2534                    2462                    2560 
##             D.nwrds.log         D.nwrds.unq.log             D.sum.TfIdf 
##                    1521                    1522                    1522 
## D.ratio.sum.TfIdf.nwrds             D.nchrs.log             D.nuppr.log 
##                    1522                    1521                    1523 
##             D.ndgts.log           D.npnct01.log           D.npnct03.log 
##                    2428                    2581                    2616 
##           D.npnct05.log           D.npnct06.log           D.npnct08.log 
##                    2594                    2556                    2583 
##           D.npnct09.log           D.npnct10.log           D.npnct11.log 
##                    2643                    2650                    2303 
##           D.npnct12.log           D.npnct13.log           D.npnct14.log 
##                    2539                    1934                    2584 
##           D.npnct15.log           D.npnct16.log           D.npnct24.log 
##                    2638                    2548                    1521 
##           D.npnct28.log         D.nstopwrds.log                D.P.mini 
##                    2651                    1664                    2625 
##                 D.P.air 
##                    2639 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## description   condition    cellular     carrier       color     storage 
##        1521           0           0           0           0           0 
## productline  prdline.my    descr.my 
##           0           0        1521
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 84.236 87.843   3.607
## 9 partition.data.training          6          0 87.843     NA      NA
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
## Fit    525    451      NA
## OOB    476    409      NA
##        sold.0    sold.1 sold.NA
##            NA        NA       1
## Fit 0.5379098 0.4620902      NA
## OOB 0.5378531 0.4621469      NA
##    prdline.my .n.Tst .n.OOB .freqRatio.Tst .freqRatio.OOB
## 3      iPad 2    154    171      0.1929825      0.1932203
## 5     iPadAir    137    152      0.1716792      0.1717514
## 4     iPad 3+    123    136      0.1541353      0.1536723
## 6    iPadmini    114    126      0.1428571      0.1423729
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
## 62         UniqueID -0.1904242            TRUE 0.1904242       <NA>
## sold.fctr sold.fctr         NA            TRUE        NA       <NA>
##           freqRatio percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## 70         1.163953     0.1074691   FALSE FALSE    FALSE            FALSE
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
## [1] 2659   69
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 1861   68
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 976  68
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 885  68
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 798  68
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
## 9  partition.data.training          6          0 87.843 88.732   0.889
## 10              fit.models          7          0 88.733     NA      NA
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
## 1                      0.471                 0.003         0.5
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

![](ebayipads_txtclstr_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6320953
## 3        0.2 0.6320953
## 4        0.3 0.6320953
## 5        0.4 0.6320953
## 6        0.5 0.4824561
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-2.png) 

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

![](ebayipads_txtclstr_files/figure-html/fit.models_0-3.png) 

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

![](ebayipads_txtclstr_files/figure-html/fit.models_0-4.png) 

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
## 1                      0.254                 0.001   0.5143786
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
## [1] "    indep_vars: biddable, startprice.log"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.488 on full training set
```

```
## Loading required package: rpart.plot
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 976 
## 
##          CP nsplit rel error
## 1 0.4878049      0         1
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
##               model_id model_method                    feats
## 1 Max.cor.Y.cv.0.rpart        rpart biddable, startprice.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.696                 0.013
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1         0.5                    0.5               0        0.5379098
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
## [1] "    indep_vars: biddable, startprice.log"
## Fitting cp = 0 on full training set
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 976 
## 
##              CP nsplit rel error
## 1  0.4878048780      0 1.0000000
## 2  0.0166297118      1 0.5121951
## 3  0.0110864745      3 0.4789357
## 4  0.0099778271      4 0.4678492
## 5  0.0066518847      6 0.4478936
## 6  0.0055432373      8 0.4345898
## 7  0.0033259424     10 0.4235033
## 8  0.0011086475     14 0.4101996
## 9  0.0007390983     20 0.4035477
## 10 0.0002217295     23 0.4013304
## 11 0.0000000000     33 0.3991131
## 
## Variable importance
## startprice.log       biddable 
##             52             48 
## 
## Node number 1: 976 observations,    complexity param=0.4878049
##   predicted class=N  expected loss=0.4620902  P(node) =1
##     class counts:   525   451
##    probabilities: 0.538 0.462 
##   left son=2 (536 obs) right son=3 (440 obs)
##   Primary splits:
##       biddable       < 0.5      to the left,  improve=132.8253, (0 missing)
##       startprice.log < 4.610145 to the right, improve=113.3071, (0 missing)
##   Surrogate splits:
##       startprice.log < 5.030417 to the right, agree=0.738, adj=0.418, (0 split)
## 
## Node number 2: 536 observations,    complexity param=0.003325942
##   predicted class=N  expected loss=0.2257463  P(node) =0.5491803
##     class counts:   415   121
##    probabilities: 0.774 0.226 
##   left son=4 (333 obs) right son=5 (203 obs)
##   Primary splits:
##       startprice.log < 5.344364 to the right, improve=6.453827, (0 missing)
## 
## Node number 3: 440 observations,    complexity param=0.01662971
##   predicted class=Y  expected loss=0.25  P(node) =0.4508197
##     class counts:   110   330
##    probabilities: 0.250 0.750 
##   left son=6 (163 obs) right son=7 (277 obs)
##   Primary splits:
##       startprice.log < 4.923459 to the right, improve=38.16294, (0 missing)
## 
## Node number 4: 333 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1651652  P(node) =0.3411885
##     class counts:   278    55
##    probabilities: 0.835 0.165 
##   left son=8 (123 obs) right son=9 (210 obs)
##   Primary splits:
##       startprice.log < 5.972491 to the right, improve=1.379799, (0 missing)
## 
## Node number 5: 203 observations,    complexity param=0.003325942
##   predicted class=N  expected loss=0.3251232  P(node) =0.2079918
##     class counts:   137    66
##    probabilities: 0.675 0.325 
##   left son=10 (137 obs) right son=11 (66 obs)
##   Primary splits:
##       startprice.log < 4.574498 to the right, improve=1.921612, (0 missing)
## 
## Node number 6: 163 observations,    complexity param=0.01662971
##   predicted class=N  expected loss=0.4785276  P(node) =0.1670082
##     class counts:    85    78
##    probabilities: 0.521 0.479 
##   left son=12 (21 obs) right son=13 (142 obs)
##   Primary splits:
##       startprice.log < 6.024113 to the right, improve=5.432188, (0 missing)
## 
## Node number 7: 277 observations
##   predicted class=Y  expected loss=0.09025271  P(node) =0.2838115
##     class counts:    25   252
##    probabilities: 0.090 0.910 
## 
## Node number 8: 123 observations
##   predicted class=N  expected loss=0.1056911  P(node) =0.1260246
##     class counts:   110    13
##    probabilities: 0.894 0.106 
## 
## Node number 9: 210 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.2  P(node) =0.2151639
##     class counts:   168    42
##    probabilities: 0.800 0.200 
##   left son=18 (200 obs) right son=19 (10 obs)
##   Primary splits:
##       startprice.log < 5.940158 to the left,  improve=1.89, (0 missing)
## 
## Node number 10: 137 observations,    complexity param=0.001108647
##   predicted class=N  expected loss=0.2773723  P(node) =0.1403689
##     class counts:    99    38
##    probabilities: 0.723 0.277 
##   left son=20 (23 obs) right son=21 (114 obs)
##   Primary splits:
##       startprice.log < 4.828274 to the left,  improve=2.004376, (0 missing)
## 
## Node number 11: 66 observations,    complexity param=0.003325942
##   predicted class=N  expected loss=0.4242424  P(node) =0.06762295
##     class counts:    38    28
##    probabilities: 0.576 0.424 
##   left son=22 (11 obs) right son=23 (55 obs)
##   Primary splits:
##       startprice.log < 3.365867 to the left,  improve=1.551515, (0 missing)
## 
## Node number 12: 21 observations
##   predicted class=N  expected loss=0.1428571  P(node) =0.02151639
##     class counts:    18     3
##    probabilities: 0.857 0.143 
## 
## Node number 13: 142 observations,    complexity param=0.01108647
##   predicted class=Y  expected loss=0.471831  P(node) =0.1454918
##     class counts:    67    75
##    probabilities: 0.472 0.528 
##   left son=26 (9 obs) right son=27 (133 obs)
##   Primary splits:
##       startprice.log < 5.010535 to the left,  improve=1.798875, (0 missing)
## 
## Node number 18: 200 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.185  P(node) =0.204918
##     class counts:   163    37
##    probabilities: 0.815 0.185 
##   left son=36 (7 obs) right son=37 (193 obs)
##   Primary splits:
##       startprice.log < 5.365942 to the left,  improve=0.4965285, (0 missing)
## 
## Node number 19: 10 observations
##   predicted class=N  expected loss=0.5  P(node) =0.0102459
##     class counts:     5     5
##    probabilities: 0.500 0.500 
## 
## Node number 20: 23 observations
##   predicted class=N  expected loss=0.08695652  P(node) =0.02356557
##     class counts:    21     2
##    probabilities: 0.913 0.087 
## 
## Node number 21: 114 observations,    complexity param=0.001108647
##   predicted class=N  expected loss=0.3157895  P(node) =0.1168033
##     class counts:    78    36
##    probabilities: 0.684 0.316 
##   left son=42 (107 obs) right son=43 (7 obs)
##   Primary splits:
##       startprice.log < 4.917424 to the right, improve=0.9747734, (0 missing)
## 
## Node number 22: 11 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.01127049
##     class counts:     9     2
##    probabilities: 0.818 0.182 
## 
## Node number 23: 55 observations,    complexity param=0.003325942
##   predicted class=N  expected loss=0.4727273  P(node) =0.05635246
##     class counts:    29    26
##    probabilities: 0.527 0.473 
##   left son=46 (35 obs) right son=47 (20 obs)
##   Primary splits:
##       startprice.log < 4.248424 to the right, improve=1.975325, (0 missing)
## 
## Node number 26: 9 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.009221311
##     class counts:     7     2
##    probabilities: 0.778 0.222 
## 
## Node number 27: 133 observations,    complexity param=0.009977827
##   predicted class=Y  expected loss=0.4511278  P(node) =0.1362705
##     class counts:    60    73
##    probabilities: 0.451 0.549 
##   left son=54 (104 obs) right son=55 (29 obs)
##   Primary splits:
##       startprice.log < 5.176086 to the right, improve=0.8381365, (0 missing)
## 
## Node number 36: 7 observations
##   predicted class=N  expected loss=0  P(node) =0.007172131
##     class counts:     7     0
##    probabilities: 1.000 0.000 
## 
## Node number 37: 193 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1917098  P(node) =0.1977459
##     class counts:   156    37
##    probabilities: 0.808 0.192 
##   left son=74 (186 obs) right son=75 (7 obs)
##   Primary splits:
##       startprice.log < 5.393605 to the right, improve=0.8150076, (0 missing)
## 
## Node number 42: 107 observations,    complexity param=0.001108647
##   predicted class=N  expected loss=0.2990654  P(node) =0.1096311
##     class counts:    75    32
##    probabilities: 0.701 0.299 
##   left son=84 (8 obs) right son=85 (99 obs)
##   Primary splits:
##       startprice.log < 5.005922 to the left,  improve=1.546682, (0 missing)
## 
## Node number 43: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.007172131
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 46: 35 observations,    complexity param=0.001108647
##   predicted class=N  expected loss=0.3714286  P(node) =0.03586066
##     class counts:    22    13
##    probabilities: 0.629 0.371 
##   left son=92 (10 obs) right son=93 (25 obs)
##   Primary splits:
##       startprice.log < 4.375675 to the left,  improve=0.8228571, (0 missing)
## 
## Node number 47: 20 observations
##   predicted class=Y  expected loss=0.35  P(node) =0.0204918
##     class counts:     7    13
##    probabilities: 0.350 0.650 
## 
## Node number 54: 104 observations,    complexity param=0.009977827
##   predicted class=Y  expected loss=0.4807692  P(node) =0.1065574
##     class counts:    50    54
##    probabilities: 0.481 0.519 
##   left son=108 (43 obs) right son=109 (61 obs)
##   Primary splits:
##       startprice.log < 5.501177 to the left,  improve=2.250183, (0 missing)
## 
## Node number 55: 29 observations
##   predicted class=Y  expected loss=0.3448276  P(node) =0.02971311
##     class counts:    10    19
##    probabilities: 0.345 0.655 
## 
## Node number 74: 186 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1827957  P(node) =0.1905738
##     class counts:   152    34
##    probabilities: 0.817 0.183 
##   left son=148 (155 obs) right son=149 (31 obs)
##   Primary splits:
##       startprice.log < 5.843472 to the left,  improve=0.4215054, (0 missing)
## 
## Node number 75: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.007172131
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 84: 8 observations
##   predicted class=N  expected loss=0  P(node) =0.008196721
##     class counts:     8     0
##    probabilities: 1.000 0.000 
## 
## Node number 85: 99 observations,    complexity param=0.001108647
##   predicted class=N  expected loss=0.3232323  P(node) =0.1014344
##     class counts:    67    32
##    probabilities: 0.677 0.323 
##   left son=170 (84 obs) right son=171 (15 obs)
##   Primary splits:
##       startprice.log < 5.023794 to the right, improve=1.56075, (0 missing)
## 
## Node number 92: 10 observations
##   predicted class=N  expected loss=0.2  P(node) =0.0102459
##     class counts:     8     2
##    probabilities: 0.800 0.200 
## 
## Node number 93: 25 observations,    complexity param=0.001108647
##   predicted class=N  expected loss=0.44  P(node) =0.02561475
##     class counts:    14    11
##    probabilities: 0.560 0.440 
##   left son=186 (18 obs) right son=187 (7 obs)
##   Primary splits:
##       startprice.log < 4.430386 to the right, improve=0.335873, (0 missing)
## 
## Node number 108: 43 observations,    complexity param=0.006651885
##   predicted class=N  expected loss=0.3953488  P(node) =0.04405738
##     class counts:    26    17
##    probabilities: 0.605 0.395 
##   left son=216 (16 obs) right son=217 (27 obs)
##   Primary splits:
##       startprice.log < 5.322712 to the right, improve=3.724806, (0 missing)
## 
## Node number 109: 61 observations,    complexity param=0.005543237
##   predicted class=Y  expected loss=0.3934426  P(node) =0.0625
##     class counts:    24    37
##    probabilities: 0.393 0.607 
##   left son=218 (46 obs) right son=219 (15 obs)
##   Primary splits:
##       startprice.log < 5.531362 to the right, improve=1.488667, (0 missing)
## 
## Node number 148: 155 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1677419  P(node) =0.1588115
##     class counts:   129    26
##    probabilities: 0.832 0.168 
##   left son=296 (33 obs) right son=297 (122 obs)
##   Primary splits:
##       startprice.log < 5.768305 to the right, improve=0.495005, (0 missing)
## 
## Node number 149: 31 observations
##   predicted class=N  expected loss=0.2580645  P(node) =0.0317623
##     class counts:    23     8
##    probabilities: 0.742 0.258 
## 
## Node number 170: 84 observations,    complexity param=0.0007390983
##   predicted class=N  expected loss=0.2857143  P(node) =0.08606557
##     class counts:    60    24
##    probabilities: 0.714 0.286 
##   left son=340 (16 obs) right son=341 (68 obs)
##   Primary splits:
##       startprice.log < 5.135337 to the left,  improve=0.3813025, (0 missing)
## 
## Node number 171: 15 observations
##   predicted class=Y  expected loss=0.4666667  P(node) =0.01536885
##     class counts:     7     8
##    probabilities: 0.467 0.533 
## 
## Node number 186: 18 observations
##   predicted class=N  expected loss=0.3888889  P(node) =0.01844262
##     class counts:    11     7
##    probabilities: 0.611 0.389 
## 
## Node number 187: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.007172131
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 216: 16 observations
##   predicted class=N  expected loss=0.125  P(node) =0.01639344
##     class counts:    14     2
##    probabilities: 0.875 0.125 
## 
## Node number 217: 27 observations,    complexity param=0.006651885
##   predicted class=Y  expected loss=0.4444444  P(node) =0.02766393
##     class counts:    12    15
##    probabilities: 0.444 0.556 
##   left son=434 (7 obs) right son=435 (20 obs)
##   Primary splits:
##       startprice.log < 5.220329 to the left,  improve=1.37619, (0 missing)
## 
## Node number 218: 46 observations,    complexity param=0.005543237
##   predicted class=Y  expected loss=0.4565217  P(node) =0.04713115
##     class counts:    21    25
##    probabilities: 0.457 0.543 
##   left son=436 (9 obs) right son=437 (37 obs)
##   Primary splits:
##       startprice.log < 5.600228 to the left,  improve=2.30957, (0 missing)
## 
## Node number 219: 15 observations
##   predicted class=Y  expected loss=0.2  P(node) =0.01536885
##     class counts:     3    12
##    probabilities: 0.200 0.800 
## 
## Node number 296: 33 observations
##   predicted class=N  expected loss=0.09090909  P(node) =0.03381148
##     class counts:    30     3
##    probabilities: 0.909 0.091 
## 
## Node number 297: 122 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1885246  P(node) =0.125
##     class counts:    99    23
##    probabilities: 0.811 0.189 
##   left son=594 (111 obs) right son=595 (11 obs)
##   Primary splits:
##       startprice.log < 5.720131 to the left,  improve=1.711161, (0 missing)
## 
## Node number 340: 16 observations
##   predicted class=N  expected loss=0.1875  P(node) =0.01639344
##     class counts:    13     3
##    probabilities: 0.812 0.188 
## 
## Node number 341: 68 observations,    complexity param=0.0007390983
##   predicted class=N  expected loss=0.3088235  P(node) =0.06967213
##     class counts:    47    21
##    probabilities: 0.691 0.309 
##   left son=682 (43 obs) right son=683 (25 obs)
##   Primary splits:
##       startprice.log < 5.227067 to the right, improve=0.6573187, (0 missing)
## 
## Node number 434: 7 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.007172131
##     class counts:     5     2
##    probabilities: 0.714 0.286 
## 
## Node number 435: 20 observations
##   predicted class=Y  expected loss=0.35  P(node) =0.0204918
##     class counts:     7    13
##    probabilities: 0.350 0.650 
## 
## Node number 436: 9 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.009221311
##     class counts:     7     2
##    probabilities: 0.778 0.222 
## 
## Node number 437: 37 observations
##   predicted class=Y  expected loss=0.3783784  P(node) =0.03790984
##     class counts:    14    23
##    probabilities: 0.378 0.622 
## 
## Node number 594: 111 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1621622  P(node) =0.1137295
##     class counts:    93    18
##    probabilities: 0.838 0.162 
##   left son=1188 (67 obs) right son=1189 (44 obs)
##   Primary splits:
##       startprice.log < 5.527188 to the right, improve=0.6180645, (0 missing)
## 
## Node number 595: 11 observations
##   predicted class=N  expected loss=0.4545455  P(node) =0.01127049
##     class counts:     6     5
##    probabilities: 0.545 0.455 
## 
## Node number 682: 43 observations
##   predicted class=N  expected loss=0.255814  P(node) =0.04405738
##     class counts:    32    11
##    probabilities: 0.744 0.256 
## 
## Node number 683: 25 observations,    complexity param=0.0007390983
##   predicted class=N  expected loss=0.4  P(node) =0.02561475
##     class counts:    15    10
##    probabilities: 0.600 0.400 
##   left son=1366 (14 obs) right son=1367 (11 obs)
##   Primary splits:
##       startprice.log < 5.192818 to the left,  improve=0.8311688, (0 missing)
## 
## Node number 1188: 67 observations
##   predicted class=N  expected loss=0.119403  P(node) =0.06864754
##     class counts:    59     8
##    probabilities: 0.881 0.119 
## 
## Node number 1189: 44 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.2272727  P(node) =0.04508197
##     class counts:    34    10
##    probabilities: 0.773 0.227 
##   left son=2378 (18 obs) right son=2379 (26 obs)
##   Primary splits:
##       startprice.log < 5.476045 to the left,  improve=0.8220668, (0 missing)
## 
## Node number 1366: 14 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.01434426
##     class counts:    10     4
##    probabilities: 0.714 0.286 
## 
## Node number 1367: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.01127049
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## Node number 2378: 18 observations
##   predicted class=N  expected loss=0.1111111  P(node) =0.01844262
##     class counts:    16     2
##    probabilities: 0.889 0.111 
## 
## Node number 2379: 26 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.3076923  P(node) =0.02663934
##     class counts:    18     8
##    probabilities: 0.692 0.308 
##   left son=4758 (19 obs) right son=4759 (7 obs)
##   Primary splits:
##       startprice.log < 5.507343 to the right, improve=1.332562, (0 missing)
## 
## Node number 4758: 19 observations
##   predicted class=N  expected loss=0.2105263  P(node) =0.01946721
##     class counts:    15     4
##    probabilities: 0.789 0.211 
## 
## Node number 4759: 7 observations
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
##      2) biddable< 0.5 536 121 N (0.77425373 0.22574627)  
##        4) startprice.log>=5.344364 333  55 N (0.83483483 0.16516517)  
##          8) startprice.log>=5.972491 123  13 N (0.89430894 0.10569106) *
##          9) startprice.log< 5.972491 210  42 N (0.80000000 0.20000000)  
##           18) startprice.log< 5.940158 200  37 N (0.81500000 0.18500000)  
##             36) startprice.log< 5.365942 7   0 N (1.00000000 0.00000000) *
##             37) startprice.log>=5.365942 193  37 N (0.80829016 0.19170984)  
##               74) startprice.log>=5.393605 186  34 N (0.81720430 0.18279570)  
##                148) startprice.log< 5.843472 155  26 N (0.83225806 0.16774194)  
##                  296) startprice.log>=5.768305 33   3 N (0.90909091 0.09090909) *
##                  297) startprice.log< 5.768305 122  23 N (0.81147541 0.18852459)  
##                    594) startprice.log< 5.720131 111  18 N (0.83783784 0.16216216)  
##                     1188) startprice.log>=5.527188 67   8 N (0.88059701 0.11940299) *
##                     1189) startprice.log< 5.527188 44  10 N (0.77272727 0.22727273)  
##                       2378) startprice.log< 5.476045 18   2 N (0.88888889 0.11111111) *
##                       2379) startprice.log>=5.476045 26   8 N (0.69230769 0.30769231)  
##                         4758) startprice.log>=5.507343 19   4 N (0.78947368 0.21052632) *
##                         4759) startprice.log< 5.507343 7   3 Y (0.42857143 0.57142857) *
##                    595) startprice.log>=5.720131 11   5 N (0.54545455 0.45454545) *
##                149) startprice.log>=5.843472 31   8 N (0.74193548 0.25806452) *
##               75) startprice.log< 5.393605 7   3 N (0.57142857 0.42857143) *
##           19) startprice.log>=5.940158 10   5 N (0.50000000 0.50000000) *
##        5) startprice.log< 5.344364 203  66 N (0.67487685 0.32512315)  
##         10) startprice.log>=4.574498 137  38 N (0.72262774 0.27737226)  
##           20) startprice.log< 4.828274 23   2 N (0.91304348 0.08695652) *
##           21) startprice.log>=4.828274 114  36 N (0.68421053 0.31578947)  
##             42) startprice.log>=4.917424 107  32 N (0.70093458 0.29906542)  
##               84) startprice.log< 5.005922 8   0 N (1.00000000 0.00000000) *
##               85) startprice.log>=5.005922 99  32 N (0.67676768 0.32323232)  
##                170) startprice.log>=5.023794 84  24 N (0.71428571 0.28571429)  
##                  340) startprice.log< 5.135337 16   3 N (0.81250000 0.18750000) *
##                  341) startprice.log>=5.135337 68  21 N (0.69117647 0.30882353)  
##                    682) startprice.log>=5.227067 43  11 N (0.74418605 0.25581395) *
##                    683) startprice.log< 5.227067 25  10 N (0.60000000 0.40000000)  
##                     1366) startprice.log< 5.192818 14   4 N (0.71428571 0.28571429) *
##                     1367) startprice.log>=5.192818 11   5 Y (0.45454545 0.54545455) *
##                171) startprice.log< 5.023794 15   7 Y (0.46666667 0.53333333) *
##             43) startprice.log< 4.917424 7   3 Y (0.42857143 0.57142857) *
##         11) startprice.log< 4.574498 66  28 N (0.57575758 0.42424242)  
##           22) startprice.log< 3.365867 11   2 N (0.81818182 0.18181818) *
##           23) startprice.log>=3.365867 55  26 N (0.52727273 0.47272727)  
##             46) startprice.log>=4.248424 35  13 N (0.62857143 0.37142857)  
##               92) startprice.log< 4.375675 10   2 N (0.80000000 0.20000000) *
##               93) startprice.log>=4.375675 25  11 N (0.56000000 0.44000000)  
##                186) startprice.log>=4.430386 18   7 N (0.61111111 0.38888889) *
##                187) startprice.log< 4.430386 7   3 Y (0.42857143 0.57142857) *
##             47) startprice.log< 4.248424 20   7 Y (0.35000000 0.65000000) *
##      3) biddable>=0.5 440 110 Y (0.25000000 0.75000000)  
##        6) startprice.log>=4.923459 163  78 N (0.52147239 0.47852761)  
##         12) startprice.log>=6.024113 21   3 N (0.85714286 0.14285714) *
##         13) startprice.log< 6.024113 142  67 Y (0.47183099 0.52816901)  
##           26) startprice.log< 5.010535 9   2 N (0.77777778 0.22222222) *
##           27) startprice.log>=5.010535 133  60 Y (0.45112782 0.54887218)  
##             54) startprice.log>=5.176086 104  50 Y (0.48076923 0.51923077)  
##              108) startprice.log< 5.501177 43  17 N (0.60465116 0.39534884)  
##                216) startprice.log>=5.322712 16   2 N (0.87500000 0.12500000) *
##                217) startprice.log< 5.322712 27  12 Y (0.44444444 0.55555556)  
##                  434) startprice.log< 5.220329 7   2 N (0.71428571 0.28571429) *
##                  435) startprice.log>=5.220329 20   7 Y (0.35000000 0.65000000) *
##              109) startprice.log>=5.501177 61  24 Y (0.39344262 0.60655738)  
##                218) startprice.log>=5.531362 46  21 Y (0.45652174 0.54347826)  
##                  436) startprice.log< 5.600228 9   2 N (0.77777778 0.22222222) *
##                  437) startprice.log>=5.600228 37  14 Y (0.37837838 0.62162162) *
##                219) startprice.log< 5.531362 15   3 Y (0.20000000 0.80000000) *
##             55) startprice.log< 5.176086 29  10 Y (0.34482759 0.65517241) *
##        7) startprice.log< 4.923459 277  25 Y (0.09025271 0.90974729) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6578171
## 3        0.2 0.7653631
## 4        0.3 0.8025478
## 5        0.4 0.8030303
## 6        0.5 0.8013245
## 7        0.6 0.7820966
## 8        0.7 0.7106326
## 9        0.8 0.7106326
## 10       0.9 0.6923077
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           423
## 2         Y                                            80
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                           102
## 2                                           371
##          Prediction
## Reference   N   Y
##         N 423 102
##         Y  80 371
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.135246e-01   6.261780e-01   7.876443e-01   8.374971e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   5.319126e-73   1.195599e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6390244
## 3        0.2 0.7014028
## 4        0.3 0.7353952
## 5        0.4 0.7395349
## 6        0.5 0.7502987
## 7        0.6 0.7634961
## 8        0.7 0.7096774
## 9        0.8 0.7096774
## 10       0.9 0.7102526
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           404
## 2         Y                                           112
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            72
## 2                                           297
##          Prediction
## Reference   N   Y
##         N 404  72
##         Y 112 297
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.920904e-01   5.788853e-01   7.638373e-01   8.183833e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   4.582845e-56   4.038763e-03 
##                    model_id model_method                    feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart biddable, startprice.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.491                  0.01
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8754662                    0.4       0.8030303        0.8135246
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7876443             0.8374971      0.626178   0.8186805
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7634961        0.7920904
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7638373             0.8183833     0.5788853
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
## Fitting cp = 0.0111 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-11.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 976 
## 
##           CP nsplit rel error
## 1 0.48780488      0 1.0000000
## 2 0.01662971      1 0.5121951
## 3 0.01108647      3 0.4789357
## 
## Variable importance
##       biddable startprice.log 
##             57             43 
## 
## Node number 1: 976 observations,    complexity param=0.4878049
##   predicted class=N  expected loss=0.4620902  P(node) =1
##     class counts:   525   451
##    probabilities: 0.538 0.462 
##   left son=2 (536 obs) right son=3 (440 obs)
##   Primary splits:
##       biddable       < 0.5      to the left,  improve=132.8253, (0 missing)
##       startprice.log < 4.610145 to the right, improve=113.3071, (0 missing)
##   Surrogate splits:
##       startprice.log < 5.030417 to the right, agree=0.738, adj=0.418, (0 split)
## 
## Node number 2: 536 observations
##   predicted class=N  expected loss=0.2257463  P(node) =0.5491803
##     class counts:   415   121
##    probabilities: 0.774 0.226 
## 
## Node number 3: 440 observations,    complexity param=0.01662971
##   predicted class=Y  expected loss=0.25  P(node) =0.4508197
##     class counts:   110   330
##    probabilities: 0.250 0.750 
##   left son=6 (163 obs) right son=7 (277 obs)
##   Primary splits:
##       startprice.log < 4.923459 to the right, improve=38.16294, (0 missing)
## 
## Node number 6: 163 observations,    complexity param=0.01662971
##   predicted class=N  expected loss=0.4785276  P(node) =0.1670082
##     class counts:    85    78
##    probabilities: 0.521 0.479 
##   left son=12 (21 obs) right son=13 (142 obs)
##   Primary splits:
##       startprice.log < 6.024113 to the right, improve=5.432188, (0 missing)
## 
## Node number 7: 277 observations
##   predicted class=Y  expected loss=0.09025271  P(node) =0.2838115
##     class counts:    25   252
##    probabilities: 0.090 0.910 
## 
## Node number 12: 21 observations
##   predicted class=N  expected loss=0.1428571  P(node) =0.02151639
##     class counts:    18     3
##    probabilities: 0.857 0.143 
## 
## Node number 13: 142 observations
##   predicted class=Y  expected loss=0.471831  P(node) =0.1454918
##     class counts:    67    75
##    probabilities: 0.472 0.528 
## 
## n= 976 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 976 451 N (0.53790984 0.46209016)  
##    2) biddable< 0.5 536 121 N (0.77425373 0.22574627) *
##    3) biddable>=0.5 440 110 Y (0.25000000 0.75000000)  
##      6) startprice.log>=4.923459 163  78 N (0.52147239 0.47852761)  
##       12) startprice.log>=6.024113 21   3 N (0.85714286 0.14285714) *
##       13) startprice.log< 6.024113 142  67 Y (0.47183099 0.52816901) *
##      7) startprice.log< 4.923459 277  25 Y (0.09025271 0.90974729) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-13.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6320953
## 3        0.2 0.6372688
## 4        0.3 0.7517241
## 5        0.4 0.7517241
## 6        0.5 0.7517241
## 7        0.6 0.6923077
## 8        0.7 0.6923077
## 9        0.8 0.6923077
## 10       0.9 0.6923077
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-14.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 433
## 2         Y                                 124
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                  92
## 2                                 327
##          Prediction
## Reference   N   Y
##         N 433  92
##         Y 124 327
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.786885e-01   5.525805e-01   7.513118e-01   8.043723e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   4.209745e-55   3.492018e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6321484
## 3        0.2 0.6311668
## 4        0.3 0.7705957
## 5        0.4 0.7705957
## 6        0.5 0.7705957
## 7        0.6 0.7102526
## 8        0.7 0.7102526
## 9        0.8 0.7102526
## 10       0.9 0.7102526
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-16.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 400
## 2         Y                                 105
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                  76
## 2                                 304
##          Prediction
## Reference   N   Y
##         N 400  76
##         Y 105 304
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.954802e-01   5.865393e-01   7.673772e-01   8.215999e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.262986e-57   3.741344e-02 
##          model_id model_method                    feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart biddable, startprice.log               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                       1.13                 0.012   0.8085735
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5       0.7517241        0.7745729
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7513118             0.8043723      0.539325   0.8157399
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7705957        0.7954802
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7673772             0.8215999     0.5865393
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02352595      0.05361153
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

![](ebayipads_txtclstr_files/figure-html/fit.models_0-17.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_0-18.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_0-19.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_0-20.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.9769  -0.7235  -0.5139   0.7920   2.0974  
## 
## Coefficients:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     2.72869    0.48878   5.583 2.37e-08 ***
## biddable        1.68289    0.16366  10.283  < 2e-16 ***
## startprice.log -0.74275    0.09048  -8.209 2.23e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  956.82  on 973  degrees of freedom
## AIC: 962.82
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6347643
## 3        0.2 0.7094536
## 4        0.3 0.7361963
## 5        0.4 0.7467249
## 6        0.5 0.7471526
## 7        0.6 0.7211896
## 8        0.7 0.6740638
## 9        0.8 0.5185185
## 10       0.9 0.3669065
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-22.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               426
## 2         Y                               123
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                                99
## 2                               328
##          Prediction
## Reference   N   Y
##         N 426  99
##         Y 123 328
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.725410e-01   5.407292e-01   7.449290e-01   7.984977e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   2.909621e-52   1.226710e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6341085
## 3        0.2 0.6978485
## 4        0.3 0.7425968
## 5        0.4 0.7701711
## 6        0.5 0.7692308
## 7        0.6 0.7530364
## 8        0.7 0.7005988
## 9        0.8 0.5580589
## 10       0.9 0.4200385
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-24.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               382
## 2         Y                                94
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                                94
## 2                               315
##          Prediction
## Reference   N   Y
##         N 382  94
##         Y  94 315
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.875706e-01   5.726922e-01   7.591217e-01   8.140900e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   5.004042e-54   1.000000e+00 
##        model_id model_method                    feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm biddable, startprice.log               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.045                 0.013   0.8349826
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5       0.7471526        0.7694604
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.744929             0.7984977     0.5343223   0.8432177
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7701711        0.7875706
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7591217               0.81409     0.5726922    962.8176
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.006501391      0.01185686
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

![](ebayipads_txtclstr_files/figure-html/fit.models_0-25.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_0-26.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_0-27.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_0-28.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.1030  -0.7121  -0.5101   0.7338   2.1051  
## 
## Coefficients:
##                                    Estimate Std. Error z value Pr(>|z|)
## (Intercept)                         2.80755    0.49295   5.695 1.23e-08
## biddable                            3.19913    1.61205   1.985 0.047199
## startprice.log                     -0.75772    0.09131  -8.299  < 2e-16
## `biddable:D.terms.n.post.stop`      0.50890    0.32530   1.564 0.117720
## `biddable:D.TfIdf.sum.post.stem`    0.35430    0.21976   1.612 0.106911
## `biddable:D.ratio.nstopwrds.nwrds` -1.23513    1.60126  -0.771 0.440501
## `biddable:D.npnct06.log`           -0.22041    0.83606  -0.264 0.792064
## `biddable:D.nchrs.log`              0.65441    1.07609   0.608 0.543096
## `biddable:D.nwrds.unq.log`         -4.61564    3.52263  -1.310 0.190101
## `biddable:cellular.fctr1`          -0.23833    0.28874  -0.825 0.409130
## `biddable:cellular.fctrUnknown`    -1.28584    0.36921  -3.483 0.000496
##                                       
## (Intercept)                        ***
## biddable                           *  
## startprice.log                     ***
## `biddable:D.terms.n.post.stop`        
## `biddable:D.TfIdf.sum.post.stem`      
## `biddable:D.ratio.nstopwrds.nwrds`    
## `biddable:D.npnct06.log`              
## `biddable:D.nchrs.log`                
## `biddable:D.nwrds.unq.log`            
## `biddable:cellular.fctr1`             
## `biddable:cellular.fctrUnknown`    ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.4  on 975  degrees of freedom
## Residual deviance:  939.7  on 965  degrees of freedom
## AIC: 961.7
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-29.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6356589
## 3        0.2 0.7100694
## 4        0.3 0.7394439
## 5        0.4 0.7461024
## 6        0.5 0.7465116
## 7        0.6 0.7358025
## 8        0.7 0.6884354
## 9        0.8 0.5466035
## 10       0.9 0.3879004
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-30.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         437
## 2         Y                                         130
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                          88
## 2                                         321
##          Prediction
## Reference   N   Y
##         N 437  88
##         Y 130 321
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.766393e-01   5.477275e-01   7.491833e-01   8.024150e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   3.810422e-54   5.488506e-03 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-31.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6350932
## 3        0.2 0.6985019
## 4        0.3 0.7428571
## 5        0.4 0.7699877
## 6        0.5 0.7639594
## 7        0.6 0.7405405
## 8        0.7 0.6960352
## 9        0.8 0.5680272
## 10       0.9 0.4423440
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-32.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         385
## 2         Y                                          96
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                          91
## 2                                         313
##          Prediction
## Reference   N   Y
##         N 385  91
##         Y  96 313
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.887006e-01   5.745991e-01   7.603001e-01   8.151638e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.563801e-54   7.698975e-01 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                      feats
## 1 biddable, startprice.log, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.122                 0.015
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8397381                    0.5       0.7465116        0.7715212
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7491833              0.802415     0.5375685   0.8383252
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7699877        0.7887006
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7603001             0.8151638     0.5745991    961.7029
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.00431088     0.009633643
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
## [1] "    indep_vars: biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-33.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_0-34.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_0-35.png) 

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
## -3.5500  -0.7026  -0.2589   0.6263   2.5408  
## 
## Coefficients: (9 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                    2.692e+00  5.156e+00
## biddable                                       1.530e+00  1.919e-01
## D.npnct15.log                                  1.544e+00  8.075e-01
## D.npnct03.log                                  1.072e+00  1.433e+00
## D.terms.n.stem.stop.Ratio                     -2.356e+00  4.621e+00
## D.ratio.sum.TfIdf.nwrds                        2.640e-01  2.040e-01
## D.npnct01.log                                 -1.489e-02  5.143e-01
## D.TfIdf.sum.stem.stop.Ratio                    2.992e+00  2.904e+00
## storage.fctr16                                 5.632e-01  5.096e-01
## storage.fctr32                                 5.260e-01  5.345e-01
## storage.fctr64                                 1.158e+00  5.228e-01
## storage.fctrUnknown                            7.495e-01  6.950e-01
## D.npnct11.log                                  1.377e-02  3.292e-01
## .rnorm                                        -1.078e-01  9.111e-02
## D.npnct10.log                                  9.812e-01  1.270e+00
## D.npnct08.log                                 -1.074e+00  7.642e-01
## `prdline.my.fctriPad 1`                        6.838e-01  5.383e-01
## `prdline.my.fctriPad 2`                        9.389e-01  5.499e-01
## `prdline.my.fctriPad 3+`                       1.149e+00  5.358e-01
## prdline.my.fctriPadAir                         1.649e+00  5.376e-01
## prdline.my.fctriPadmini                        1.082e+00  5.125e-01
## `prdline.my.fctriPadmini 2+`                   1.340e+00  5.630e-01
## color.fctrBlack                               -5.560e-02  2.536e-01
## color.fctrGold                                -1.218e-01  4.885e-01
## `color.fctrSpace Gray`                         1.570e-01  3.140e-01
## color.fctrWhite                                1.321e-01  2.451e-01
## D.npnct06.log                                 -1.691e+00  9.979e-01
## D.npnct28.log                                 -3.250e+00  1.092e+03
## D.npnct12.log                                  3.158e-01  7.144e-01
## D.npnct09.log                                 -7.960e+00  5.629e+02
## D.ndgts.log                                    1.764e-01  3.779e-01
## cellular.fctr1                                 1.478e-01  2.098e-01
## cellular.fctrUnknown                          -4.177e-01  4.403e-01
## D.npnct14.log                                 -8.234e-01  8.020e-01
## D.terms.n.post.stop                           -4.091e-02  4.406e-02
## D.npnct05.log                                 -2.776e+00  1.710e+00
## `condition.fctrFor parts or not working`      -4.343e-01  4.018e-01
## `condition.fctrManufacturer refurbished`      -1.196e-01  6.643e-01
## condition.fctrNew                             -4.050e-01  3.071e-01
## `condition.fctrNew other (see details)`       -3.215e-01  4.053e-01
## `condition.fctrSeller refurbished`            -1.210e+00  4.569e-01
## idseq.my                                      -5.732e-04  1.969e-04
## startprice.log                                -1.037e+00  1.420e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.934e+00  7.186e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       1.873e+00  8.703e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       6.671e-02  7.341e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      1.365e-01  6.702e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`      2.034e-01  5.439e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     2.388e-01  8.045e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2` -1.262e-01  9.269e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -1.071e+00  1.233e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`       4.711e-02  8.143e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -3.192e-01  1.049e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -1.426e+00  7.829e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`     -7.459e-01  8.012e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`     3.309e-01  7.098e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  1.414e-01  8.373e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -2.035e+00  1.264e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`       1.588e+00  1.047e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      4.430e-01  8.234e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -3.391e-01  8.521e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`       2.857e+00  1.497e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`            NA         NA
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                     0.522  0.60158    
## biddable                                        7.976 1.51e-15 ***
## D.npnct15.log                                   1.912  0.05584 .  
## D.npnct03.log                                   0.748  0.45464    
## D.terms.n.stem.stop.Ratio                      -0.510  0.61016    
## D.ratio.sum.TfIdf.nwrds                         1.294  0.19558    
## D.npnct01.log                                  -0.029  0.97691    
## D.TfIdf.sum.stem.stop.Ratio                     1.031  0.30276    
## storage.fctr16                                  1.105  0.26911    
## storage.fctr32                                  0.984  0.32509    
## storage.fctr64                                  2.215  0.02673 *  
## storage.fctrUnknown                             1.078  0.28087    
## D.npnct11.log                                   0.042  0.96663    
## .rnorm                                         -1.184  0.23661    
## D.npnct10.log                                   0.773  0.43963    
## D.npnct08.log                                  -1.405  0.15988    
## `prdline.my.fctriPad 1`                         1.270  0.20399    
## `prdline.my.fctriPad 2`                         1.707  0.08774 .  
## `prdline.my.fctriPad 3+`                        2.144  0.03201 *  
## prdline.my.fctriPadAir                          3.067  0.00216 ** 
## prdline.my.fctriPadmini                         2.110  0.03482 *  
## `prdline.my.fctriPadmini 2+`                    2.379  0.01735 *  
## color.fctrBlack                                -0.219  0.82646    
## color.fctrGold                                 -0.249  0.80302    
## `color.fctrSpace Gray`                          0.500  0.61696    
## color.fctrWhite                                 0.539  0.59007    
## D.npnct06.log                                  -1.695  0.09016 .  
## D.npnct28.log                                  -0.003  0.99763    
## D.npnct12.log                                   0.442  0.65841    
## D.npnct09.log                                  -0.014  0.98872    
## D.ndgts.log                                     0.467  0.64057    
## cellular.fctr1                                  0.704  0.48119    
## cellular.fctrUnknown                           -0.949  0.34274    
## D.npnct14.log                                  -1.027  0.30458    
## D.terms.n.post.stop                            -0.928  0.35317    
## D.npnct05.log                                  -1.624  0.10442    
## `condition.fctrFor parts or not working`       -1.081  0.27985    
## `condition.fctrManufacturer refurbished`       -0.180  0.85709    
## condition.fctrNew                              -1.319  0.18722    
## `condition.fctrNew other (see details)`        -0.793  0.42768    
## `condition.fctrSeller refurbished`             -2.649  0.00807 ** 
## idseq.my                                       -2.912  0.00359 ** 
## startprice.log                                 -7.304 2.79e-13 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.692  0.00710 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        2.152  0.03143 *  
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.091  0.92760    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`       0.204  0.83861    
## `prdline.my.fctriPadAir:.clusterid.fctr2`       0.374  0.70842    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      0.297  0.76657    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  -0.136  0.89172    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.869  0.38499    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.058  0.95386    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.304  0.76088    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -1.822  0.06851 .  
## `prdline.my.fctriPadAir:.clusterid.fctr3`      -0.931  0.35186    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.466  0.64110    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.169  0.86588    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.610  0.10742    
## `prdline.my.fctriPad 2:.clusterid.fctr4`        1.516  0.12945    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       0.538  0.59060    
## `prdline.my.fctriPadAir:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.398  0.69063    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        1.908  0.05636 .  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`         NA       NA    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  844.29  on 914  degrees of freedom
## AIC: 968.29
## 
## Number of Fisher Scoring iterations: 14
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-36.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_0-37.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6862897
## 3        0.2 0.7349823
## 4        0.3 0.7676969
## 5        0.4 0.7873501
## 6        0.5 0.7819026
## 7        0.6 0.7699877
## 8        0.7 0.7327249
## 9        0.8 0.6480938
## 10       0.9 0.4497445
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               420
## 2         Y                                90
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                               105
## 2                               361
##          Prediction
## Reference   N   Y
##         N 420 105
##         Y  90 361
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.002049e-01   5.990393e-01   7.737180e-01   8.248664e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   8.793905e-66   3.160728e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-38.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_0-39.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6701031
## 3        0.2 0.7142857
## 4        0.3 0.7411504
## 5        0.4 0.7542169
## 6        0.5 0.7566879
## 7        0.6 0.7537012
## 8        0.7 0.7149059
## 9        0.8 0.6334405
## 10       0.9 0.4872727
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_0-40.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               397
## 2         Y                               112
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                79
## 2                               297
##          Prediction
## Reference   N   Y
##         N 397  79
##         Y 112 297
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.841808e-01   5.633942e-01   7.555883e-01   8.108669e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.574968e-52   2.058893e-02 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.569                 0.147
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8759624                    0.4       0.7873501        0.7694604
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.773718             0.8248664     0.5340042   0.8422572
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7566879        0.7841808
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7555883             0.8108669     0.5633942    968.2921
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0222869      0.04529799
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 10 fit.models          7          0  88.733 115.609  26.876
## 11 fit.models          7          1 115.609      NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor     bgn end elapsed
## 1 fit.models_1_bgn          1          0 120.179  NA      NA
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
## 1 fit.models_1_bgn          1          0 120.179 120.195   0.016
## 2 fit.models_1_glm          2          0 120.195      NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-1.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_1-2.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_1-3.png) 

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
## -3.7462  -0.6704  -0.2158   0.6248   2.4156  
## 
## Coefficients: (11 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                    5.536e+01  4.181e+01
## biddable                                       1.514e+00  1.979e-01
## D.ratio.nstopwrds.nwrds                       -8.003e+00  6.821e+00
## D.npnct15.log                                  1.165e+00  8.973e-01
## D.npnct03.log                                  6.260e-01  1.653e+00
## D.terms.n.stem.stop.Ratio                     -3.943e+01  3.878e+01
## D.ratio.sum.TfIdf.nwrds                        9.293e-01  8.155e-01
## D.npnct01.log                                 -1.585e-01  6.888e-01
## D.TfIdf.sum.stem.stop.Ratio                   -4.088e+00  1.851e+01
## storage.fctr16                                 2.827e-01  5.220e-01
## storage.fctr32                                 1.828e-01  5.497e-01
## storage.fctr64                                 9.087e-01  5.356e-01
## storage.fctrUnknown                            3.848e-01  7.089e-01
## D.npnct11.log                                  4.513e-02  3.979e-01
## .rnorm                                        -1.075e-01  9.336e-02
## D.npnct10.log                                  7.603e-01  1.435e+00
## D.TfIdf.sum.post.stop                         -1.019e+00  2.866e+00
## D.TfIdf.sum.post.stem                          1.214e+00  2.975e+00
## D.sum.TfIdf                                           NA         NA
## D.npnct13.log                                  4.547e-02  4.322e-01
## D.npnct08.log                                 -9.263e-01  8.488e-01
## `prdline.my.fctriPad 1`                        6.981e-01  5.432e-01
## `prdline.my.fctriPad 2`                        9.111e-01  5.575e-01
## `prdline.my.fctriPad 3+`                       1.137e+00  5.463e-01
## prdline.my.fctriPadAir                         1.646e+00  5.420e-01
## prdline.my.fctriPadmini                        1.035e+00  5.197e-01
## `prdline.my.fctriPadmini 2+`                   1.391e+00  5.666e-01
## color.fctrBlack                               -8.010e-02  2.621e-01
## color.fctrGold                                -1.894e-01  5.037e-01
## `color.fctrSpace Gray`                         2.295e-01  3.270e-01
## color.fctrWhite                                1.028e-01  2.535e-01
## D.npnct16.log                                 -8.671e-01  2.395e+00
## D.npnct24.log                                 -8.892e+00  7.858e+00
## D.nstopwrds.log                                2.335e+00  2.109e+00
## D.npnct06.log                                 -3.463e-01  2.618e+00
## D.npnct28.log                                 -3.155e+00  1.085e+03
## D.nuppr.log                                   -1.614e+00  5.719e+00
## D.npnct12.log                                  7.972e-01  7.937e-01
## D.nchrs.log                                    1.460e+00  6.792e+00
## D.nwrds.log                                    6.034e-01  3.155e+00
## D.npnct09.log                                 -7.748e+00  5.500e+02
## D.ndgts.log                                   -2.359e-01  5.260e-01
## D.nwrds.unq.log                               -2.243e+00  3.810e+00
## `carrier.fctrAT&T`                             3.344e-01  6.757e-01
## carrier.fctrOther                              1.556e+01  9.828e+02
## carrier.fctrSprint                             7.053e-01  9.142e-01
## `carrier.fctrT-Mobile`                        -1.615e+00  1.372e+00
## carrier.fctrUnknown                           -3.439e-01  4.451e-01
## carrier.fctrVerizon                            8.329e-01  7.208e-01
## cellular.fctr1                                -1.394e-01  6.175e-01
## cellular.fctrUnknown                                  NA         NA
## D.npnct14.log                                 -1.285e+00  9.458e-01
## D.terms.n.post.stem                            4.478e+00  4.196e+00
## D.terms.n.post.stop                           -4.593e+00  4.147e+00
## D.npnct05.log                                 -2.451e+00  1.889e+00
## `condition.fctrFor parts or not working`      -5.360e-01  4.150e-01
## `condition.fctrManufacturer refurbished`      -2.779e-01  6.870e-01
## condition.fctrNew                             -3.484e-01  3.138e-01
## `condition.fctrNew other (see details)`       -6.156e-01  4.472e-01
## `condition.fctrSeller refurbished`            -1.323e+00  4.835e-01
## idseq.my                                      -6.508e-04  2.012e-04
## startprice.log                                -1.066e+00  1.426e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`      2.259e+00  7.914e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       1.994e+00  9.516e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       2.547e-01  8.301e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      4.355e-01  7.735e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`      6.766e-01  6.239e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     6.816e-01  8.771e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  4.213e-02  9.930e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -1.077e+00  1.341e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`       1.820e-01  9.061e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`       6.574e-03  1.128e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -1.010e+00  8.113e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`     -1.665e-01  9.087e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`     5.991e-01  8.172e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  4.958e-01  8.981e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.938e+00  1.427e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`       2.095e+00  1.143e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      9.122e-01  9.028e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -1.582e-01  9.030e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`       3.616e+00  1.675e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`            NA         NA
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                     1.324  0.18554    
## biddable                                        7.650 2.01e-14 ***
## D.ratio.nstopwrds.nwrds                        -1.173  0.24069    
## D.npnct15.log                                   1.298  0.19428    
## D.npnct03.log                                   0.379  0.70489    
## D.terms.n.stem.stop.Ratio                      -1.017  0.30925    
## D.ratio.sum.TfIdf.nwrds                         1.140  0.25444    
## D.npnct01.log                                  -0.230  0.81804    
## D.TfIdf.sum.stem.stop.Ratio                    -0.221  0.82520    
## storage.fctr16                                  0.541  0.58819    
## storage.fctr32                                  0.333  0.73946    
## storage.fctr64                                  1.697  0.08973 .  
## storage.fctrUnknown                             0.543  0.58729    
## D.npnct11.log                                   0.113  0.90972    
## .rnorm                                         -1.151  0.24966    
## D.npnct10.log                                   0.530  0.59627    
## D.TfIdf.sum.post.stop                          -0.356  0.72207    
## D.TfIdf.sum.post.stem                           0.408  0.68331    
## D.sum.TfIdf                                        NA       NA    
## D.npnct13.log                                   0.105  0.91621    
## D.npnct08.log                                  -1.091  0.27510    
## `prdline.my.fctriPad 1`                         1.285  0.19876    
## `prdline.my.fctriPad 2`                         1.634  0.10219    
## `prdline.my.fctriPad 3+`                        2.081  0.03747 *  
## prdline.my.fctriPadAir                          3.037  0.00239 ** 
## prdline.my.fctriPadmini                         1.992  0.04638 *  
## `prdline.my.fctriPadmini 2+`                    2.454  0.01411 *  
## color.fctrBlack                                -0.306  0.75989    
## color.fctrGold                                 -0.376  0.70690    
## `color.fctrSpace Gray`                          0.702  0.48274    
## color.fctrWhite                                 0.405  0.68519    
## D.npnct16.log                                  -0.362  0.71736    
## D.npnct24.log                                  -1.132  0.25783    
## D.nstopwrds.log                                 1.108  0.26803    
## D.npnct06.log                                  -0.132  0.89476    
## D.npnct28.log                                  -0.003  0.99768    
## D.nuppr.log                                    -0.282  0.77778    
## D.npnct12.log                                   1.004  0.31517    
## D.nchrs.log                                     0.215  0.82982    
## D.nwrds.log                                     0.191  0.84835    
## D.npnct09.log                                  -0.014  0.98876    
## D.ndgts.log                                    -0.449  0.65377    
## D.nwrds.unq.log                                -0.589  0.55608    
## `carrier.fctrAT&T`                              0.495  0.62070    
## carrier.fctrOther                               0.016  0.98737    
## carrier.fctrSprint                              0.772  0.44041    
## `carrier.fctrT-Mobile`                         -1.177  0.23920    
## carrier.fctrUnknown                            -0.773  0.43977    
## carrier.fctrVerizon                             1.156  0.24786    
## cellular.fctr1                                 -0.226  0.82145    
## cellular.fctrUnknown                               NA       NA    
## D.npnct14.log                                  -1.358  0.17436    
## D.terms.n.post.stem                             1.067  0.28594    
## D.terms.n.post.stop                            -1.107  0.26810    
## D.npnct05.log                                  -1.297  0.19452    
## `condition.fctrFor parts or not working`       -1.292  0.19652    
## `condition.fctrManufacturer refurbished`       -0.405  0.68583    
## condition.fctrNew                              -1.110  0.26687    
## `condition.fctrNew other (see details)`        -1.376  0.16870    
## `condition.fctrSeller refurbished`             -2.736  0.00623 ** 
## idseq.my                                       -3.235  0.00122 ** 
## startprice.log                                 -7.476 7.66e-14 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.855  0.00431 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        2.095  0.03616 *  
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.307  0.75894    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`       0.563  0.57343    
## `prdline.my.fctriPadAir:.clusterid.fctr2`       1.085  0.27813    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      0.777  0.43705    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.042  0.96616    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.803  0.42177    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.201  0.84079    
## `prdline.my.fctriPad 2:.clusterid.fctr3`        0.006  0.99535    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -1.245  0.21331    
## `prdline.my.fctriPadAir:.clusterid.fctr3`      -0.183  0.85461    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.733  0.46346    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.552  0.58091    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.358  0.17445    
## `prdline.my.fctriPad 2:.clusterid.fctr4`        1.832  0.06697 .  
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.010  0.31227    
## `prdline.my.fctriPadAir:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.175  0.86089    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.158  0.03090 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`         NA       NA    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  818.61  on 897  degrees of freedom
## AIC: 976.61
## 
## Number of Fisher Scoring iterations: 14
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-4.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6942924
## 3        0.2 0.7475292
## 4        0.3 0.7816550
## 5        0.4 0.7866379
## 6        0.5 0.7898383
## 7        0.6 0.7658537
## 8        0.7 0.7375328
## 9        0.8 0.6530612
## 10       0.9 0.5008237
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           452                            73
## 2         Y                           109                           342
##          Prediction
## Reference   N   Y
##         N 452  73
##         Y 109 342
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.135246e-01   6.227707e-01   7.876443e-01   8.374971e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   5.319126e-73   9.476372e-03 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-6.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6666667
## 3        0.2 0.7051793
## 4        0.3 0.7453142
## 5        0.4 0.7482014
## 6        0.5 0.7579214
## 7        0.6 0.7489933
## 8        0.7 0.7120116
## 9        0.8 0.6275753
## 10       0.9 0.4882459
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           395                            81
## 2         Y                           110                           299
##          Prediction
## Reference   N   Y
##         N 395  81
##         Y 110 299
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.841808e-01   5.636962e-01   7.555883e-01   8.108669e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.574968e-52   4.276387e-02 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.758                 0.223
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8857185                    0.5       0.7898383        0.7479534
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7876443             0.8374971     0.4918447   0.8336946
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7579214        0.7841808
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7555883             0.8108669     0.5636962    976.6092
##   max.AccuracySD.fit max.KappaSD.fit
## 1           0.020122      0.04144768
##                   label step_major step_minor     bgn     end elapsed
## 2      fit.models_1_glm          2          0 120.195 125.913   5.718
## 3 fit.models_1_bayesglm          3          0 125.913      NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
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

![](ebayipads_txtclstr_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.6351  -0.6867  -0.2474   0.6476   2.4349  
## 
## Coefficients:
##                                                 Estimate Std. Error
## (Intercept)                                    1.6093742  5.9965521
## biddable                                       1.4988608  0.1907966
## D.ratio.nstopwrds.nwrds                       -0.0538953  2.2330403
## D.npnct15.log                                  1.3192660  0.8437038
## D.npnct03.log                                  0.3864288  1.4658048
## D.terms.n.stem.stop.Ratio                     -0.3903113  4.9949044
## D.ratio.sum.TfIdf.nwrds                        0.4330915  0.4116716
## D.npnct01.log                                  0.0553444  0.5201218
## D.TfIdf.sum.stem.stop.Ratio                    2.7028079  3.2097616
## storage.fctr16                                 0.1825621  0.4462198
## storage.fctr32                                 0.1088834  0.4712663
## storage.fctr64                                 0.7984017  0.4610258
## storage.fctrUnknown                            0.2825683  0.6135998
## D.npnct11.log                                  0.1546530  0.3401696
## .rnorm                                        -0.1059893  0.0903461
## D.npnct10.log                                  0.9814212  1.3080068
## D.TfIdf.sum.post.stop                          0.0361852  0.2898697
## D.TfIdf.sum.post.stem                          0.0655480  0.3044014
## D.sum.TfIdf                                    0.0655480  0.3044014
## D.npnct13.log                                  0.0741962  0.3386545
## D.npnct08.log                                 -0.8822568  0.7707937
## `prdline.my.fctriPad 1`                        0.4262509  0.4707734
## `prdline.my.fctriPad 2`                        0.6758982  0.4780416
## `prdline.my.fctriPad 3+`                       0.8449236  0.4612554
## prdline.my.fctriPadAir                         1.2818283  0.4626224
## prdline.my.fctriPadmini                        0.7600070  0.4451360
## `prdline.my.fctriPadmini 2+`                   1.0262616  0.4886957
## color.fctrBlack                               -0.0781245  0.2491955
## color.fctrGold                                -0.1523348  0.4711781
## `color.fctrSpace Gray`                         0.2139915  0.3115309
## color.fctrWhite                                0.1033658  0.2421058
## D.npnct16.log                                 -0.2105483  1.8776510
## D.npnct24.log                                 -1.4762762  2.5247607
## D.nstopwrds.log                                0.6584321  0.6459895
## D.npnct06.log                                 -0.9923665  2.0664790
## D.npnct28.log                                 -0.0608860  2.1818535
## D.nuppr.log                                   -0.0639651  0.4828287
## D.npnct12.log                                  0.6465259  0.7229673
## D.nchrs.log                                   -0.0595596  0.4745632
## D.nwrds.log                                    0.2088547  0.7623855
## D.npnct09.log                                 -1.8760218  5.0237320
## D.ndgts.log                                    0.1273040  0.3781245
## D.nwrds.unq.log                               -0.2725193  0.9679498
## `carrier.fctrAT&T`                             0.0985416  0.7633931
## carrier.fctrOther                              1.4835057  1.6750826
## carrier.fctrSprint                             0.4179600  0.9114095
## `carrier.fctrT-Mobile`                        -1.4558981  1.1459941
## carrier.fctrUnknown                           -0.6068707  0.7798876
## carrier.fctrVerizon                            0.5094110  0.7891074
## cellular.fctr1                                 0.0844622  0.7390138
## cellular.fctrUnknown                           0.1996713  0.8503078
## D.npnct14.log                                 -0.8252301  0.8038002
## D.terms.n.post.stem                           -0.0541243  0.1906172
## D.terms.n.post.stop                           -0.0806750  0.1896245
## D.npnct05.log                                 -2.2712463  1.4609335
## `condition.fctrFor parts or not working`      -0.4687060  0.3891782
## `condition.fctrManufacturer refurbished`      -0.2158004  0.6293988
## condition.fctrNew                             -0.3426252  0.3019158
## `condition.fctrNew other (see details)`       -0.4773338  0.4125077
## `condition.fctrSeller refurbished`            -1.1195745  0.4438587
## idseq.my                                      -0.0006302  0.0001942
## startprice.log                                -1.0033696  0.1338942
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.7476439  0.6682132
## `prdline.my.fctriPad 1:.clusterid.fctr2`       1.5903689  0.7965303
## `prdline.my.fctriPad 2:.clusterid.fctr2`       0.0738332  0.6843114
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      0.1805603  0.6431012
## `prdline.my.fctriPadAir:.clusterid.fctr2`      0.4216945  0.5097120
## `prdline.my.fctriPadmini:.clusterid.fctr2`     0.3428717  0.7334903
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2` -0.1648895  0.8300327
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -1.2135537  0.9950049
## `prdline.my.fctriPad 1:.clusterid.fctr3`       0.0719646  0.7583699
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -0.3352433  0.9100186
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -1.1338614  0.6877935
## `prdline.my.fctriPadAir:.clusterid.fctr3`     -0.3443634  0.7480520
## `prdline.my.fctriPadmini:.clusterid.fctr3`     0.3110076  0.6766586
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  0.2565962  0.7555686
## `prdline.my.fctrUnknown:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.7091155  1.1140496
## `prdline.my.fctriPad 2:.clusterid.fctr4`       1.4154726  0.9418006
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      0.5535973  0.7679931
## `prdline.my.fctriPadAir:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -0.3193088  0.7658544
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`  0.0000000  2.5000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`       0.0000000  2.5000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`       2.4073371  1.2648489
## `prdline.my.fctriPad 3+:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadAir:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`     0.0000000  2.5000000
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`  0.0000000  2.5000000
##                                               z value Pr(>|z|)    
## (Intercept)                                     0.268  0.78840    
## biddable                                        7.856 3.97e-15 ***
## D.ratio.nstopwrds.nwrds                        -0.024  0.98074    
## D.npnct15.log                                   1.564  0.11790    
## D.npnct03.log                                   0.264  0.79207    
## D.terms.n.stem.stop.Ratio                      -0.078  0.93772    
## D.ratio.sum.TfIdf.nwrds                         1.052  0.29279    
## D.npnct01.log                                   0.106  0.91526    
## D.TfIdf.sum.stem.stop.Ratio                     0.842  0.39976    
## storage.fctr16                                  0.409  0.68244    
## storage.fctr32                                  0.231  0.81728    
## storage.fctr64                                  1.732  0.08331 .  
## storage.fctrUnknown                             0.461  0.64515    
## D.npnct11.log                                   0.455  0.64937    
## .rnorm                                         -1.173  0.24074    
## D.npnct10.log                                   0.750  0.45306    
## D.TfIdf.sum.post.stop                           0.125  0.90066    
## D.TfIdf.sum.post.stem                           0.215  0.82951    
## D.sum.TfIdf                                     0.215  0.82951    
## D.npnct13.log                                   0.219  0.82658    
## D.npnct08.log                                  -1.145  0.25237    
## `prdline.my.fctriPad 1`                         0.905  0.36524    
## `prdline.my.fctriPad 2`                         1.414  0.15739    
## `prdline.my.fctriPad 3+`                        1.832  0.06698 .  
## prdline.my.fctriPadAir                          2.771  0.00559 ** 
## prdline.my.fctriPadmini                         1.707  0.08776 .  
## `prdline.my.fctriPadmini 2+`                    2.100  0.03573 *  
## color.fctrBlack                                -0.314  0.75390    
## color.fctrGold                                 -0.323  0.74646    
## `color.fctrSpace Gray`                          0.687  0.49214    
## color.fctrWhite                                 0.427  0.66942    
## D.npnct16.log                                  -0.112  0.91072    
## D.npnct24.log                                  -0.585  0.55874    
## D.nstopwrds.log                                 1.019  0.30808    
## D.npnct06.log                                  -0.480  0.63107    
## D.npnct28.log                                  -0.028  0.97774    
## D.nuppr.log                                    -0.132  0.89460    
## D.npnct12.log                                   0.894  0.37118    
## D.nchrs.log                                    -0.126  0.90012    
## D.nwrds.log                                     0.274  0.78412    
## D.npnct09.log                                  -0.373  0.70883    
## D.ndgts.log                                     0.337  0.73636    
## D.nwrds.unq.log                                -0.282  0.77829    
## `carrier.fctrAT&T`                              0.129  0.89729    
## carrier.fctrOther                               0.886  0.37582    
## carrier.fctrSprint                              0.459  0.64653    
## `carrier.fctrT-Mobile`                         -1.270  0.20393    
## carrier.fctrUnknown                            -0.778  0.43648    
## carrier.fctrVerizon                             0.646  0.51857    
## cellular.fctr1                                  0.114  0.90901    
## cellular.fctrUnknown                            0.235  0.81435    
## D.npnct14.log                                  -1.027  0.30458    
## D.terms.n.post.stem                            -0.284  0.77645    
## D.terms.n.post.stop                            -0.425  0.67051    
## D.npnct05.log                                  -1.555  0.12003    
## `condition.fctrFor parts or not working`       -1.204  0.22846    
## `condition.fctrManufacturer refurbished`       -0.343  0.73170    
## condition.fctrNew                              -1.135  0.25644    
## `condition.fctrNew other (see details)`        -1.157  0.24721    
## `condition.fctrSeller refurbished`             -2.522  0.01166 *  
## idseq.my                                       -3.245  0.00117 ** 
## startprice.log                                 -7.494 6.69e-14 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.615  0.00891 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        1.997  0.04587 *  
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.108  0.91408    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`       0.281  0.77889    
## `prdline.my.fctriPadAir:.clusterid.fctr2`       0.827  0.40806    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      0.467  0.64018    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  -0.199  0.84253    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -1.220  0.22260    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.095  0.92440    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.368  0.71258    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -1.649  0.09924 .  
## `prdline.my.fctriPadAir:.clusterid.fctr3`      -0.460  0.64527    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.460  0.64579    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.340  0.73415    
## `prdline.my.fctrUnknown:.clusterid.fctr4`       0.000  1.00000    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.534  0.12499    
## `prdline.my.fctriPad 2:.clusterid.fctr4`        1.503  0.13285    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       0.721  0.47101    
## `prdline.my.fctriPadAir:.clusterid.fctr4`       0.000  1.00000    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.417  0.67673    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`   0.000  1.00000    
## `prdline.my.fctrUnknown:.clusterid.fctr5`       0.000  1.00000    
## `prdline.my.fctriPad 1:.clusterid.fctr5`        0.000  1.00000    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        1.903  0.05701 .  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`       0.000  1.00000    
## `prdline.my.fctriPadAir:.clusterid.fctr5`       0.000  1.00000    
## `prdline.my.fctriPadmini:.clusterid.fctr5`      0.000  1.00000    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`   0.000  1.00000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  825.32  on 886  degrees of freedom
## AIC: 1005.3
## 
## Number of Fisher Scoring iterations: 13
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6861538
## 3        0.2 0.7439786
## 4        0.3 0.7814045
## 5        0.4 0.7874865
## 6        0.5 0.7870370
## 7        0.6 0.7711138
## 8        0.7 0.7318362
## 9        0.8 0.6401180
## 10       0.9 0.4628378
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                414
## 2         Y                                 86
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                111
## 2                                365
##          Prediction
## Reference   N   Y
##         N 414 111
##         Y  86 365
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.981557e-01   5.955574e-01   7.715794e-01   8.229192e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   1.029234e-64   8.727897e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6649573
## 3        0.2 0.7027559
## 4        0.3 0.7382256
## 5        0.4 0.7497034
## 6        0.5 0.7629911
## 7        0.6 0.7462687
## 8        0.7 0.7138728
## 9        0.8 0.6485623
## 10       0.9 0.4751381
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                397
## 2         Y                                108
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 79
## 2                                301
##          Prediction
## Reference   N   Y
##         N 397  79
##         Y 108 301
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.887006e-01   5.728335e-01   7.603001e-01   8.151638e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.563801e-54   4.060286e-02 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.733                 0.353
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8837884                    0.4       0.7874865        0.7643574
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7715794             0.8229192     0.5240255   0.8381377
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7629911        0.7887006
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7603001             0.8151638     0.5728335     1005.32
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01350949      0.02837009
##                   label step_major step_minor     bgn     end elapsed
## 3 fit.models_1_bayesglm          3          0 125.913 131.975   6.062
## 4   fit.models_1_glmnet          4          0 131.977      NA      NA
## [1] "fitting model: All.X.glmnet"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
```

```
## Loading required package: glmnet
## Loaded glmnet 2.0-2
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-12.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting alpha = 1, lambda = 0.00522 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: alpha
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-13.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_1-14.png) 

```
##             Length Class      Mode     
## a0            88   -none-     numeric  
## beta        7832   dgCMatrix  S4       
## df            88   -none-     numeric  
## dim            2   -none-     numeric  
## lambda        88   -none-     numeric  
## dev.ratio     88   -none-     numeric  
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
##                             1.8900717570 
##                                 biddable 
##                             1.5037576368 
##                  D.ratio.nstopwrds.nwrds 
##                             0.1576130765 
##                            D.npnct15.log 
##                             0.9245967014 
##                  D.ratio.sum.TfIdf.nwrds 
##                             0.1737126235 
##              D.TfIdf.sum.stem.stop.Ratio 
##                             1.5399281727 
##                           storage.fctr32 
##                            -0.0052527236 
##                           storage.fctr64 
##                             0.4941470349 
##                                   .rnorm 
##                            -0.0577775052 
##                            D.npnct10.log 
##                             0.1885858448 
##                            D.npnct08.log 
##                            -0.4282753476 
##                   prdline.my.fctriPadAir 
##                             0.3551828700 
##               prdline.my.fctriPadmini 2+ 
##                             0.0631384683 
##                          color.fctrBlack 
##                            -0.0051601727 
##                           color.fctrGold 
##                            -0.0615302359 
##                     color.fctrSpace Gray 
##                             0.0013839523 
##                            D.npnct16.log 
##                            -0.0069269523 
##                          D.nstopwrds.log 
##                             0.0784223739 
##                            D.npnct06.log 
##                            -0.7418151740 
##                            D.npnct09.log 
##                            -0.1221542037 
##                        carrier.fctrOther 
##                             1.5252770531 
##                       carrier.fctrSprint 
##                             0.1631320757 
##                     carrier.fctrT-Mobile 
##                            -1.0312426831 
##                      carrier.fctrUnknown 
##                            -0.5739899111 
##                      carrier.fctrVerizon 
##                             0.3309891035 
##                            D.npnct14.log 
##                            -0.3612016664 
##                      D.terms.n.post.stem 
##                            -0.0068408215 
##                      D.terms.n.post.stop 
##                            -0.0015350160 
##                            D.npnct05.log 
##                            -1.7458619725 
##   condition.fctrFor parts or not working 
##                            -0.0205819320 
##                        condition.fctrNew 
##                            -0.1992191888 
##    condition.fctrNew other (see details) 
##                            -0.0341264887 
##         condition.fctrSeller refurbished 
##                            -0.7667076340 
##                                 idseq.my 
##                            -0.0005744527 
##                           startprice.log 
##                            -0.7846415889 
##  prdline.my.fctrUnknown:.clusterid.fctr2 
##                             0.6715936455 
##   prdline.my.fctriPad 1:.clusterid.fctr2 
##                             0.9139927843 
##  prdline.my.fctrUnknown:.clusterid.fctr3 
##                            -1.4263821416 
##   prdline.my.fctriPad 2:.clusterid.fctr3 
##                            -0.3829329880 
##  prdline.my.fctriPad 3+:.clusterid.fctr3 
##                            -0.9392130525 
##  prdline.my.fctriPadAir:.clusterid.fctr3 
##                            -0.2166638792 
## prdline.my.fctriPadmini:.clusterid.fctr3 
##                             0.0779762393 
##   prdline.my.fctriPad 1:.clusterid.fctr4 
##                            -1.7911913749 
##   prdline.my.fctriPad 2:.clusterid.fctr4 
##                             0.6079200980 
## prdline.my.fctriPadmini:.clusterid.fctr4 
##                            -0.2460724556 
##   prdline.my.fctriPad 2:.clusterid.fctr5 
##                             1.8295937022 
## [1] "max lambda < lambdaOpt:"
##                                 (Intercept) 
##                                5.250484e+00 
##                                    biddable 
##                                1.515508e+00 
##                     D.ratio.nstopwrds.nwrds 
##                               -2.819908e+00 
##                               D.npnct15.log 
##                                1.341165e+00 
##                               D.npnct03.log 
##                                2.179556e-01 
##                   D.terms.n.stem.stop.Ratio 
##                               -1.101602e+00 
##                     D.ratio.sum.TfIdf.nwrds 
##                                8.725416e-01 
##                               D.npnct01.log 
##                                2.415178e-02 
##                 D.TfIdf.sum.stem.stop.Ratio 
##                                2.535756e+00 
##                              storage.fctr16 
##                                2.170484e-01 
##                              storage.fctr32 
##                                1.265112e-01 
##                              storage.fctr64 
##                                8.542114e-01 
##                         storage.fctrUnknown 
##                                3.461242e-01 
##                               D.npnct11.log 
##                                1.142126e-01 
##                                      .rnorm 
##                               -1.124571e-01 
##                               D.npnct10.log 
##                                1.042119e+00 
##                       D.TfIdf.sum.post.stem 
##                                1.695755e-01 
##                                 D.sum.TfIdf 
##                                2.086871e-05 
##                               D.npnct13.log 
##                                6.304493e-02 
##                               D.npnct08.log 
##                               -1.008270e+00 
##                       prdline.my.fctriPad 1 
##                                6.647621e-01 
##                       prdline.my.fctriPad 2 
##                                8.963112e-01 
##                      prdline.my.fctriPad 3+ 
##                                1.140257e+00 
##                      prdline.my.fctriPadAir 
##                                1.603994e+00 
##                     prdline.my.fctriPadmini 
##                                1.021537e+00 
##                  prdline.my.fctriPadmini 2+ 
##                                1.351934e+00 
##                             color.fctrBlack 
##                               -7.662760e-02 
##                              color.fctrGold 
##                               -1.567421e-01 
##                        color.fctrSpace Gray 
##                                2.221189e-01 
##                             color.fctrWhite 
##                                9.749425e-02 
##                               D.npnct16.log 
##                               -5.711164e-01 
##                               D.npnct24.log 
##                               -6.697289e+00 
##                             D.nstopwrds.log 
##                                1.263494e+00 
##                               D.npnct06.log 
##                               -5.547379e-01 
##                               D.npnct28.log 
##                               -1.706268e-01 
##                                 D.nuppr.log 
##                               -2.269069e-02 
##                               D.npnct12.log 
##                                7.539988e-01 
##                                 D.nwrds.log 
##                                5.045958e-01 
##                               D.npnct09.log 
##                               -2.198356e+00 
##                                 D.ndgts.log 
##                                8.777980e-02 
##                             D.nwrds.unq.log 
##                               -1.054006e-03 
##                            carrier.fctrAT&T 
##                                2.038886e-01 
##                           carrier.fctrOther 
##                                6.242576e+00 
##                          carrier.fctrSprint 
##                                5.682754e-01 
##                        carrier.fctrT-Mobile 
##                               -1.750407e+00 
##                         carrier.fctrUnknown 
##                               -5.126390e-01 
##                         carrier.fctrVerizon 
##                                6.231067e-01 
##                        cellular.fctrUnknown 
##                                1.698305e-01 
##                               D.npnct14.log 
##                               -9.689062e-01 
##                         D.terms.n.post.stop 
##                               -2.846895e-01 
##                               D.npnct05.log 
##                               -2.822585e+00 
##      condition.fctrFor parts or not working 
##                               -5.479304e-01 
##      condition.fctrManufacturer refurbished 
##                               -2.313443e-01 
##                           condition.fctrNew 
##                               -3.462771e-01 
##       condition.fctrNew other (see details) 
##                               -5.856121e-01 
##            condition.fctrSeller refurbished 
##                               -1.302055e+00 
##                                    idseq.my 
##                               -6.397574e-04 
##                              startprice.log 
##                               -1.058744e+00 
##     prdline.my.fctrUnknown:.clusterid.fctr2 
##                                2.216047e+00 
##      prdline.my.fctriPad 1:.clusterid.fctr2 
##                                1.985049e+00 
##      prdline.my.fctriPad 2:.clusterid.fctr2 
##                                3.012247e-01 
##     prdline.my.fctriPad 3+:.clusterid.fctr2 
##                                3.943365e-01 
##     prdline.my.fctriPadAir:.clusterid.fctr2 
##                                6.668514e-01 
##    prdline.my.fctriPadmini:.clusterid.fctr2 
##                                6.637483e-01 
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                               -4.271051e-02 
##     prdline.my.fctrUnknown:.clusterid.fctr3 
##                               -1.275417e+00 
##      prdline.my.fctriPad 1:.clusterid.fctr3 
##                                2.141204e-01 
##      prdline.my.fctriPad 2:.clusterid.fctr3 
##                               -1.974849e-01 
##     prdline.my.fctriPad 3+:.clusterid.fctr3 
##                               -1.119579e+00 
##     prdline.my.fctriPadAir:.clusterid.fctr3 
##                               -2.005069e-01 
##    prdline.my.fctriPadmini:.clusterid.fctr3 
##                                5.139348e-01 
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                                5.136925e-01 
##      prdline.my.fctriPad 1:.clusterid.fctr4 
##                               -2.054524e+00 
##      prdline.my.fctriPad 2:.clusterid.fctr4 
##                                2.027235e+00 
##     prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                8.099551e-01 
##    prdline.my.fctriPadmini:.clusterid.fctr4 
##                               -1.826654e-01 
##      prdline.my.fctriPad 2:.clusterid.fctr5 
##                                3.390637e+00 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6671642
## 3        0.2 0.7310705
## 4        0.3 0.7549407
## 5        0.4 0.7813853
## 6        0.5 0.7670915
## 7        0.6 0.7642680
## 8        0.7 0.7090663
## 9        0.8 0.5833333
## 10       0.9 0.4077329
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-16.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              413
## 2         Y                               90
##   sold.fctr.predict.All.X.glmnet.Y
## 1                              112
## 2                              361
##          Prediction
## Reference   N   Y
##         N 413 112
##         Y  90 361
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.930328e-01   5.850986e-01   7.662373e-01   8.180470e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   4.319139e-62   1.395270e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-17.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6508972
## 3        0.2 0.7011494
## 4        0.3 0.7393675
## 5        0.4 0.7532777
## 6        0.5 0.7635904
## 7        0.6 0.7604871
## 8        0.7 0.7243402
## 9        0.8 0.5848739
## 10       0.9 0.4402277
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              396
## 2         Y                              107
##   sold.fctr.predict.All.X.glmnet.Y
## 1                               80
## 2                              302
##          Prediction
## Reference   N   Y
##         N 396  80
##         Y 107 302
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.887006e-01   5.729811e-01   7.603001e-01   8.151638e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.563801e-54   5.726164e-02 
##       model_id model_method
## 1 All.X.glmnet       glmnet
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                      5.256                 0.711
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8740745                    0.4       0.7813853        0.7725468
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7662373              0.818047     0.5404278   0.8437519
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7635904        0.7887006
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7603001             0.8151638     0.5729811
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01045785      0.02170456
##                 label step_major step_minor     bgn    end elapsed
## 4 fit.models_1_glmnet          4          0 131.977 141.37   9.394
## 5  fit.models_1_rpart          5          0 141.371     NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0111 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-19.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_1-20.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 976 
## 
##           CP nsplit rel error
## 1 0.48780488      0 1.0000000
## 2 0.04434590      1 0.5121951
## 3 0.01108647      3 0.4235033
## 
## Variable importance
##                               biddable 
##                                     43 
##                         startprice.log 
##                                     31 
##                               idseq.my 
##                                     14 
##                 prdline.my.fctriPadAir 
##                                      2 
## condition.fctrFor parts or not working 
##                                      2 
##                      condition.fctrNew 
##                                      2 
##                   color.fctrSpace Gray 
##                                      1 
##                  prdline.my.fctriPad 2 
##                                      1 
##             prdline.my.fctriPadmini 2+ 
##                                      1 
##                          D.npnct03.log 
##                                      1 
##  condition.fctrNew other (see details) 
##                                      1 
##                    storage.fctrUnknown 
##                                      1 
## 
## Node number 1: 976 observations,    complexity param=0.4878049
##   predicted class=N  expected loss=0.4620902  P(node) =1
##     class counts:   525   451
##    probabilities: 0.538 0.462 
##   left son=2 (536 obs) right son=3 (440 obs)
##   Primary splits:
##       biddable                < 0.5       to the left,  improve=132.825300, (0 missing)
##       startprice.log          < 4.610145  to the right, improve=113.307100, (0 missing)
##       idseq.my                < 876.5     to the right, improve= 40.260140, (0 missing)
##       condition.fctrNew       < 0.5       to the right, improve= 12.703470, (0 missing)
##       D.ratio.nstopwrds.nwrds < 0.269697  to the left,  improve=  7.749863, (0 missing)
##   Surrogate splits:
##       startprice.log                         < 5.030417  to the right, agree=0.738, adj=0.418, (0 split)
##       idseq.my                               < 869.5     to the right, agree=0.634, adj=0.189, (0 split)
##       condition.fctrFor parts or not working < 0.5       to the left,  agree=0.567, adj=0.039, (0 split)
##       prdline.my.fctriPad 2                  < 0.5       to the left,  agree=0.565, adj=0.034, (0 split)
##       D.npnct03.log                          < 0.3465736 to the left,  agree=0.557, adj=0.018, (0 split)
## 
## Node number 2: 536 observations
##   predicted class=N  expected loss=0.2257463  P(node) =0.5491803
##     class counts:   415   121
##    probabilities: 0.774 0.226 
## 
## Node number 3: 440 observations,    complexity param=0.0443459
##   predicted class=Y  expected loss=0.25  P(node) =0.4508197
##     class counts:   110   330
##    probabilities: 0.250 0.750 
##   left son=6 (163 obs) right son=7 (277 obs)
##   Primary splits:
##       startprice.log       < 4.923459  to the right, improve=38.162940, (0 missing)
##       idseq.my             < 920.5     to the right, improve=19.234430, (0 missing)
##       condition.fctrNew    < 0.5       to the right, improve= 3.803542, (0 missing)
##       cellular.fctrUnknown < 0.5       to the right, improve= 3.795302, (0 missing)
##       carrier.fctrUnknown  < 0.5       to the right, improve= 2.690217, (0 missing)
##   Surrogate splits:
##       prdline.my.fctriPadAir                < 0.5       to the right, agree=0.700, adj=0.190, (0 split)
##       condition.fctrNew                     < 0.5       to the right, agree=0.684, adj=0.147, (0 split)
##       color.fctrSpace Gray                  < 0.5       to the right, agree=0.675, adj=0.123, (0 split)
##       prdline.my.fctriPadmini 2+            < 0.5       to the right, agree=0.666, adj=0.098, (0 split)
##       condition.fctrNew other (see details) < 0.5       to the right, agree=0.652, adj=0.061, (0 split)
## 
## Node number 6: 163 observations,    complexity param=0.0443459
##   predicted class=N  expected loss=0.4785276  P(node) =0.1670082
##     class counts:    85    78
##    probabilities: 0.521 0.479 
##   left son=12 (68 obs) right son=13 (95 obs)
##   Primary splits:
##       idseq.my                                < 920.5     to the right, improve=17.345980, (0 missing)
##       prdline.my.fctriPadAir                  < 0.5       to the left,  improve= 5.716666, (0 missing)
##       startprice.log                          < 6.024113  to the right, improve= 5.432188, (0 missing)
##       color.fctrSpace Gray                    < 0.5       to the left,  improve= 4.308136, (0 missing)
##       prdline.my.fctriPadAir:.clusterid.fctr2 < 0.5       to the left,  improve= 3.108759, (0 missing)
##   Surrogate splits:
##       storage.fctrUnknown                    < 0.5       to the right, agree=0.626, adj=0.103, (0 split)
##       startprice.log                         < 6.024113  to the right, agree=0.626, adj=0.103, (0 split)
##       condition.fctrFor parts or not working < 0.5       to the right, agree=0.620, adj=0.088, (0 split)
##       D.ratio.sum.TfIdf.nwrds                < 0.369052  to the right, agree=0.613, adj=0.074, (0 split)
##       cellular.fctrUnknown                   < 0.5       to the right, agree=0.613, adj=0.074, (0 split)
## 
## Node number 7: 277 observations
##   predicted class=Y  expected loss=0.09025271  P(node) =0.2838115
##     class counts:    25   252
##    probabilities: 0.090 0.910 
## 
## Node number 12: 68 observations
##   predicted class=N  expected loss=0.2058824  P(node) =0.06967213
##     class counts:    54    14
##    probabilities: 0.794 0.206 
## 
## Node number 13: 95 observations
##   predicted class=Y  expected loss=0.3263158  P(node) =0.09733607
##     class counts:    31    64
##    probabilities: 0.326 0.674 
## 
## n= 976 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 976 451 N (0.53790984 0.46209016)  
##    2) biddable< 0.5 536 121 N (0.77425373 0.22574627) *
##    3) biddable>=0.5 440 110 Y (0.25000000 0.75000000)  
##      6) startprice.log>=4.923459 163  78 N (0.52147239 0.47852761)  
##       12) idseq.my>=920.5 68  14 N (0.79411765 0.20588235) *
##       13) idseq.my< 920.5 95  31 Y (0.32631579 0.67368421) *
##      7) startprice.log< 4.923459 277  25 Y (0.09025271 0.90974729) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6320953
## 3        0.2 0.6320953
## 4        0.3 0.7679222
## 5        0.4 0.7679222
## 6        0.5 0.7679222
## 7        0.6 0.7679222
## 8        0.7 0.6923077
## 9        0.8 0.6923077
## 10       0.9 0.6923077
## 11       1.0 0.0000000
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-22.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      469
## 2         Y                                      135
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                       56
## 2                                      316
##          Prediction
## Reference   N   Y
##         N 469  56
##         Y 135 316
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.043033e-01   6.014231e-01   7.779982e-01   8.287575e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   5.945293e-68   1.662581e-08 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6321484
## 3        0.2 0.6321484
## 4        0.3 0.7742782
## 5        0.4 0.7742782
## 6        0.5 0.7742782
## 7        0.6 0.7742782
## 8        0.7 0.7102526
## 9        0.8 0.7102526
## 10       0.9 0.7102526
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      418
## 2         Y                                      114
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                       58
## 2                                      295
##          Prediction
## Reference   N   Y
##         N 418  58
##         Y 114 295
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.056497e-01   6.052550e-01   7.780145e-01   8.312324e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.814281e-62   2.744280e-05 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.594                 0.069
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8116461                    0.6       0.7679222        0.8022338
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7779982             0.8287575     0.5966918   0.8087593
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7742782        0.8056497
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7780145             0.8312324      0.605255
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01883486      0.04031858
##                label step_major step_minor     bgn     end elapsed
## 5 fit.models_1_rpart          5          0 141.371 146.554   5.183
## 6    fit.models_1_rf          6          0 146.554      NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
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

![](ebayipads_txtclstr_files/figure-html/fit.models_1-24.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 45 on full training set
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-25.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_1-26.png) 

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
## importance        88   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                976   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            88   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-27.png) 

```
##    threshold    f.score
## 1        0.0 0.63209530
## 2        0.1 0.80535714
## 3        0.2 0.93568465
## 4        0.3 0.98043478
## 5        0.4 0.99778761
## 6        0.5 1.00000000
## 7        0.6 0.99889012
## 8        0.7 0.94255569
## 9        0.8 0.84948980
## 10       0.9 0.74547983
## 11       1.0 0.09302326
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-28.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   525
## 2         Y                                    NA
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                    NA
## 2                                   451
##          Prediction
## Reference   N   Y
##         N 525   0
##         Y   0 451
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.962275e-01   1.000000e+00   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##  1.487504e-263            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-29.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6672241
## 3        0.2 0.7081712
## 4        0.3 0.7480748
## 5        0.4 0.7757576
## 6        0.5 0.7870968
## 7        0.6 0.7804878
## 8        0.7 0.7542857
## 9        0.8 0.7108434
## 10       0.9 0.5939597
## 11       1.0 0.0383693
```

![](ebayipads_txtclstr_files/figure-html/fit.models_1-30.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   415
## 2         Y                                   104
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                    61
## 2                                   305
##          Prediction
## Reference   N   Y
##         N 415  61
##         Y 104 305
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.135593e-01   6.221736e-01   7.863067e-01   8.387053e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   2.079362e-66   1.076633e-03 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     16.129                 5.105
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.7858518
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9962275                     1     0.5657736   0.8532596
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7870968        0.8135593
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7863067             0.8387053     0.6221736
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02279738      0.04385952
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
## Low.cor.X.glm                                                                                                                                                                                                                                                          biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glm                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.bayesglm            biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glmnet              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rpart              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rf                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                       0                      0.471
## Random.myrandom_classfr                 0                      0.254
## Max.cor.Y.cv.0.rpart                    0                      0.696
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.491
## Max.cor.Y.rpart                         3                      1.130
## Max.cor.Y.glm                           1                      1.045
## Interact.High.cor.Y.glm                 1                      1.122
## Low.cor.X.glm                           1                      1.569
## All.X.glm                               1                      1.758
## All.X.bayesglm                          1                      2.733
## All.X.glmnet                            9                      5.256
## All.X.no.rnorm.rpart                    3                      1.594
## All.X.no.rnorm.rf                       3                     16.129
##                           min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                         0.003   0.5000000
## Random.myrandom_classfr                   0.001   0.5143786
## Max.cor.Y.cv.0.rpart                      0.013   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.010   0.8754662
## Max.cor.Y.rpart                           0.012   0.8085735
## Max.cor.Y.glm                             0.013   0.8349826
## Interact.High.cor.Y.glm                   0.015   0.8397381
## Low.cor.X.glm                             0.147   0.8759624
## All.X.glm                                 0.223   0.8857185
## All.X.bayesglm                            0.353   0.8837884
## All.X.glmnet                              0.711   0.8740745
## All.X.no.rnorm.rpart                      0.069   0.8116461
## All.X.no.rnorm.rf                         5.105   1.0000000
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6320953
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.4       0.8030303
## Max.cor.Y.rpart                              0.5       0.7517241
## Max.cor.Y.glm                                0.5       0.7471526
## Interact.High.cor.Y.glm                      0.5       0.7465116
## Low.cor.X.glm                                0.4       0.7873501
## All.X.glm                                    0.5       0.7898383
## All.X.bayesglm                               0.4       0.7874865
## All.X.glmnet                                 0.4       0.7813853
## All.X.no.rnorm.rpart                         0.6       0.7679222
## All.X.no.rnorm.rf                            0.5       1.0000000
##                           max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                0.5379098             0.5060449
## Random.myrandom_classfr          0.4620902             0.4304545
## Max.cor.Y.cv.0.rpart             0.5379098             0.5060449
## Max.cor.Y.cv.0.cp.0.rpart        0.8135246             0.7876443
## Max.cor.Y.rpart                  0.7745729             0.7513118
## Max.cor.Y.glm                    0.7694604             0.7449290
## Interact.High.cor.Y.glm          0.7715212             0.7491833
## Low.cor.X.glm                    0.7694604             0.7737180
## All.X.glm                        0.7479534             0.7876443
## All.X.bayesglm                   0.7643574             0.7715794
## All.X.glmnet                     0.7725468             0.7662373
## All.X.no.rnorm.rpart             0.8022338             0.7779982
## All.X.no.rnorm.rf                0.7858518             0.9962275
##                           max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                     0.5695455     0.0000000   0.5000000
## Random.myrandom_classfr               0.4939551     0.0000000   0.5214656
## Max.cor.Y.cv.0.rpart                  0.5695455     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8374971     0.6261780   0.8186805
## Max.cor.Y.rpart                       0.8043723     0.5393250   0.8157399
## Max.cor.Y.glm                         0.7984977     0.5343223   0.8432177
## Interact.High.cor.Y.glm               0.8024150     0.5375685   0.8383252
## Low.cor.X.glm                         0.8248664     0.5340042   0.8422572
## All.X.glm                             0.8374971     0.4918447   0.8336946
## All.X.bayesglm                        0.8229192     0.5240255   0.8381377
## All.X.glmnet                          0.8180470     0.5404278   0.8437519
## All.X.no.rnorm.rpart                  0.8287575     0.5966918   0.8087593
## All.X.no.rnorm.rf                     1.0000000     0.5657736   0.8532596
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6321484
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.6       0.7634961
## Max.cor.Y.rpart                              0.5       0.7705957
## Max.cor.Y.glm                                0.4       0.7701711
## Interact.High.cor.Y.glm                      0.4       0.7699877
## Low.cor.X.glm                                0.5       0.7566879
## All.X.glm                                    0.5       0.7579214
## All.X.bayesglm                               0.5       0.7629911
## All.X.glmnet                                 0.5       0.7635904
## All.X.no.rnorm.rpart                         0.6       0.7742782
## All.X.no.rnorm.rf                            0.5       0.7870968
##                           max.Accuracy.OOB max.AccuracyLower.OOB
## MFO.myMFO_classfr                0.5378531             0.5043611
## Random.myrandom_classfr          0.4621469             0.4289077
## Max.cor.Y.cv.0.rpart             0.5378531             0.5043611
## Max.cor.Y.cv.0.cp.0.rpart        0.7920904             0.7638373
## Max.cor.Y.rpart                  0.7954802             0.7673772
## Max.cor.Y.glm                    0.7875706             0.7591217
## Interact.High.cor.Y.glm          0.7887006             0.7603001
## Low.cor.X.glm                    0.7841808             0.7555883
## All.X.glm                        0.7841808             0.7555883
## All.X.bayesglm                   0.7887006             0.7603001
## All.X.glmnet                     0.7887006             0.7603001
## All.X.no.rnorm.rpart             0.8056497             0.7780145
## All.X.no.rnorm.rf                0.8135593             0.7863067
##                           max.AccuracyUpper.OOB max.Kappa.OOB
## MFO.myMFO_classfr                     0.5710923     0.0000000
## Random.myrandom_classfr               0.4956389     0.0000000
## Max.cor.Y.cv.0.rpart                  0.5710923     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8183833     0.5788853
## Max.cor.Y.rpart                       0.8215999     0.5865393
## Max.cor.Y.glm                         0.8140900     0.5726922
## Interact.High.cor.Y.glm               0.8151638     0.5745991
## Low.cor.X.glm                         0.8108669     0.5633942
## All.X.glm                             0.8108669     0.5636962
## All.X.bayesglm                        0.8151638     0.5728335
## All.X.glmnet                          0.8151638     0.5729811
## All.X.no.rnorm.rpart                  0.8312324     0.6052550
## All.X.no.rnorm.rf                     0.8387053     0.6221736
##                           max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                         NA              NA          NA
## Random.myrandom_classfr                   NA              NA          NA
## Max.cor.Y.cv.0.rpart                      NA              NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA              NA          NA
## Max.cor.Y.rpart                  0.023525952     0.053611532          NA
## Max.cor.Y.glm                    0.006501391     0.011856863    962.8176
## Interact.High.cor.Y.glm          0.004310880     0.009633643    961.7029
## Low.cor.X.glm                    0.022286901     0.045297990    968.2921
## All.X.glm                        0.020121996     0.041447684    976.6092
## All.X.bayesglm                   0.013509488     0.028370090   1005.3201
## All.X.glmnet                     0.010457854     0.021704560          NA
## All.X.no.rnorm.rpart             0.018834860     0.040318577          NA
## All.X.no.rnorm.rf                0.022797378     0.043859524          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 6  fit.models_1_rf          6          0 146.554 166.147  19.594
## 7 fit.models_1_end          7          0 166.148      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1 115.609 166.155  50.546
## 12 fit.models          7          2 166.156      NA      NA
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
## Low.cor.X.glm                                                                                                                                                                                                                                                          biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glm                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.bayesglm            biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glmnet              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rpart              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rf                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##                           max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                       0   0.5000000
## Random.myrandom_classfr                 0   0.5143786
## Max.cor.Y.cv.0.rpart                    0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart               0   0.8754662
## Max.cor.Y.rpart                         3   0.8085735
## Max.cor.Y.glm                           1   0.8349826
## Interact.High.cor.Y.glm                 1   0.8397381
## Low.cor.X.glm                           1   0.8759624
## All.X.glm                               1   0.8857185
## All.X.bayesglm                          1   0.8837884
## All.X.glmnet                            9   0.8740745
## All.X.no.rnorm.rpart                    3   0.8116461
## All.X.no.rnorm.rf                       3   1.0000000
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6320953
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.4       0.8030303
## Max.cor.Y.rpart                              0.5       0.7517241
## Max.cor.Y.glm                                0.5       0.7471526
## Interact.High.cor.Y.glm                      0.5       0.7465116
## Low.cor.X.glm                                0.4       0.7873501
## All.X.glm                                    0.5       0.7898383
## All.X.bayesglm                               0.4       0.7874865
## All.X.glmnet                                 0.4       0.7813853
## All.X.no.rnorm.rpart                         0.6       0.7679222
## All.X.no.rnorm.rf                            0.5       1.0000000
##                           max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                0.5379098     0.0000000   0.5000000
## Random.myrandom_classfr          0.4620902     0.0000000   0.5214656
## Max.cor.Y.cv.0.rpart             0.5379098     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart        0.8135246     0.6261780   0.8186805
## Max.cor.Y.rpart                  0.7745729     0.5393250   0.8157399
## Max.cor.Y.glm                    0.7694604     0.5343223   0.8432177
## Interact.High.cor.Y.glm          0.7715212     0.5375685   0.8383252
## Low.cor.X.glm                    0.7694604     0.5340042   0.8422572
## All.X.glm                        0.7479534     0.4918447   0.8336946
## All.X.bayesglm                   0.7643574     0.5240255   0.8381377
## All.X.glmnet                     0.7725468     0.5404278   0.8437519
## All.X.no.rnorm.rpart             0.8022338     0.5966918   0.8087593
## All.X.no.rnorm.rf                0.7858518     0.5657736   0.8532596
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6321484
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.6       0.7634961
## Max.cor.Y.rpart                              0.5       0.7705957
## Max.cor.Y.glm                                0.4       0.7701711
## Interact.High.cor.Y.glm                      0.4       0.7699877
## Low.cor.X.glm                                0.5       0.7566879
## All.X.glm                                    0.5       0.7579214
## All.X.bayesglm                               0.5       0.7629911
## All.X.glmnet                                 0.5       0.7635904
## All.X.no.rnorm.rpart                         0.6       0.7742782
## All.X.no.rnorm.rf                            0.5       0.7870968
##                           max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                0.5378531     0.0000000
## Random.myrandom_classfr          0.4621469     0.0000000
## Max.cor.Y.cv.0.rpart             0.5378531     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart        0.7920904     0.5788853
## Max.cor.Y.rpart                  0.7954802     0.5865393
## Max.cor.Y.glm                    0.7875706     0.5726922
## Interact.High.cor.Y.glm          0.7887006     0.5745991
## Low.cor.X.glm                    0.7841808     0.5633942
## All.X.glm                        0.7841808     0.5636962
## All.X.bayesglm                   0.7887006     0.5728335
## All.X.glmnet                     0.7887006     0.5729811
## All.X.no.rnorm.rpart             0.8056497     0.6052550
## All.X.no.rnorm.rf                0.8135593     0.6221736
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.myMFO_classfr                         2.12314225           333.3333333
## Random.myrandom_classfr                   3.93700787          1000.0000000
## Max.cor.Y.cv.0.rpart                      1.43678161            76.9230769
## Max.cor.Y.cv.0.cp.0.rpart                 2.03665988           100.0000000
## Max.cor.Y.rpart                           0.88495575            83.3333333
## Max.cor.Y.glm                             0.95693780            76.9230769
## Interact.High.cor.Y.glm                   0.89126560            66.6666667
## Low.cor.X.glm                             0.63734863             6.8027211
## All.X.glm                                 0.56882821             4.4843049
## All.X.bayesglm                            0.36589828             2.8328612
## All.X.glmnet                              0.19025875             1.4064698
## All.X.no.rnorm.rpart                      0.62735257            14.4927536
## All.X.no.rnorm.rf                         0.06200012             0.1958864
##                            inv.aic.fit
## MFO.myMFO_classfr                   NA
## Random.myrandom_classfr             NA
## Max.cor.Y.cv.0.rpart                NA
## Max.cor.Y.cv.0.cp.0.rpart           NA
## Max.cor.Y.rpart                     NA
## Max.cor.Y.glm             0.0010386183
## Interact.High.cor.Y.glm   0.0010398222
## Low.cor.X.glm             0.0010327463
## All.X.glm                 0.0010239510
## All.X.bayesglm            0.0009947081
## All.X.glmnet                        NA
## All.X.no.rnorm.rpart                NA
## All.X.no.rnorm.rf                   NA
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

![](ebayipads_txtclstr_files/figure-html/fit.models_2-1.png) 

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

![](ebayipads_txtclstr_files/figure-html/fit.models_2-2.png) 

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
## 13         All.X.no.rnorm.rf        0.8135593   0.8532596     0.6221736
## 12      All.X.no.rnorm.rpart        0.8056497   0.8087593     0.6052550
## 5            Max.cor.Y.rpart        0.7954802   0.8157399     0.5865393
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7920904   0.8186805     0.5788853
## 11              All.X.glmnet        0.7887006   0.8437519     0.5729811
## 7    Interact.High.cor.Y.glm        0.7887006   0.8383252     0.5745991
## 10            All.X.bayesglm        0.7887006   0.8381377     0.5728335
## 6              Max.cor.Y.glm        0.7875706   0.8432177     0.5726922
## 8              Low.cor.X.glm        0.7841808   0.8422572     0.5633942
## 9                  All.X.glm        0.7841808   0.8336946     0.5636962
## 1          MFO.myMFO_classfr        0.5378531   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.5378531   0.5000000     0.0000000
## 2    Random.myrandom_classfr        0.4621469   0.5214656     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 13          NA                    0.5
## 12          NA                    0.6
## 5           NA                    0.5
## 4           NA                    0.6
## 11          NA                    0.5
## 7     961.7029                    0.4
## 10   1005.3201                    0.5
## 6     962.8176                    0.4
## 8     968.2921                    0.5
## 9     976.6092                    0.5
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

![](ebayipads_txtclstr_files/figure-html/fit.models_2-3.png) 

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

![](ebayipads_txtclstr_files/figure-html/fit.models_2-4.png) 

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
## importance        88   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                976   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            88   -none-     character
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
## startprice.log                              1.000000e+02
## biddable                                    7.999853e+01
## idseq.my                                    6.494363e+01
## D.ratio.sum.TfIdf.nwrds                     7.135951e+00
## D.TfIdf.sum.stem.stop.Ratio                 6.720453e+00
## color.fctrBlack                             5.495087e+00
## D.ratio.nstopwrds.nwrds                     5.004874e+00
## storage.fctr64                              4.740955e+00
## D.TfIdf.sum.post.stop                       4.719678e+00
## D.TfIdf.sum.post.stem                       4.522182e+00
## color.fctrWhite                             4.187605e+00
## D.sum.TfIdf                                 4.149523e+00
## prdline.my.fctriPad 1                       4.143156e+00
## color.fctrSpace Gray                        4.062558e+00
## condition.fctrNew                           3.987112e+00
## prdline.my.fctriPadAir                      3.986975e+00
## storage.fctr16                              3.937344e+00
## cellular.fctr1                              3.714775e+00
## D.nuppr.log                                 3.588109e+00
## D.nchrs.log                                 3.447178e+00
## prdline.my.fctriPad 3+                      3.442736e+00
## prdline.my.fctriPadmini                     3.336970e+00
## cellular.fctrUnknown                        3.200631e+00
## carrier.fctrAT&T                            3.092894e+00
## D.nstopwrds.log                             3.090547e+00
## D.nwrds.log                                 3.090401e+00
## carrier.fctrUnknown                         3.084186e+00
## carrier.fctrVerizon                         2.796045e+00
## storage.fctrUnknown                         2.673223e+00
## storage.fctr32                              2.612027e+00
## condition.fctrNew other (see details)       2.464048e+00
## D.terms.n.post.stop                         2.304155e+00
## condition.fctrSeller refurbished            2.231088e+00
## D.terms.n.post.stem                         2.216511e+00
## D.npnct11.log                               2.167865e+00
## prdline.my.fctriPadmini 2+                  1.978667e+00
## prdline.my.fctriPad 2                       1.961556e+00
## D.nwrds.unq.log                             1.946190e+00
## D.npnct13.log                               1.726664e+00
## condition.fctrFor parts or not working      1.555871e+00
## color.fctrGold                              1.455401e+00
## condition.fctrManufacturer refurbished      1.388559e+00
## prdline.my.fctrUnknown:.clusterid.fctr2     1.157005e+00
## prdline.my.fctriPad 2:.clusterid.fctr5      1.112577e+00
## D.terms.n.stem.stop.Ratio                   1.019426e+00
## D.ndgts.log                                 9.524259e-01
## carrier.fctrSprint                          9.491356e-01
## D.npnct15.log                               8.008516e-01
## prdline.my.fctriPad 1:.clusterid.fctr4      7.351143e-01
## prdline.my.fctriPad 2:.clusterid.fctr4      7.158364e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2     6.913168e-01
## prdline.my.fctriPadAir:.clusterid.fctr2     6.529339e-01
## carrier.fctrT-Mobile                        6.218184e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 6.151461e-01
## D.npnct05.log                               5.705584e-01
## D.npnct08.log                               5.581608e-01
## prdline.my.fctriPadmini:.clusterid.fctr4    5.403950e-01
## D.npnct14.log                               5.369901e-01
## prdline.my.fctriPadmini:.clusterid.fctr3    4.758212e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3     4.646786e-01
## D.npnct10.log                               4.455789e-01
## D.npnct01.log                               4.160190e-01
## prdline.my.fctriPad 1:.clusterid.fctr2      4.141739e-01
## prdline.my.fctrUnknown:.clusterid.fctr3     3.997075e-01
## D.npnct24.log                               3.864426e-01
## D.npnct12.log                               3.781070e-01
## prdline.my.fctriPad 2:.clusterid.fctr2      3.420448e-01
## D.npnct16.log                               3.238302e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 3.106229e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4     2.918164e-01
## prdline.my.fctriPadAir:.clusterid.fctr3     2.847269e-01
## carrier.fctrOther                           2.227022e-01
## prdline.my.fctriPad 1:.clusterid.fctr3      2.014511e-01
## prdline.my.fctriPadmini:.clusterid.fctr2    1.790224e-01
## D.npnct03.log                               1.256671e-01
## prdline.my.fctriPad 2:.clusterid.fctr3      1.214698e-01
## D.npnct06.log                               1.005843e-01
## D.npnct28.log                               2.723279e-03
## D.npnct09.log                               2.450951e-03
## prdline.my.fctrUnknown:.clusterid.fctr4     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4 0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadmini:.clusterid.fctr5    0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5 0.000000e+00
##                                             All.X.no.rnorm.rf.importance
## startprice.log                                              1.000000e+02
## biddable                                                    7.999853e+01
## idseq.my                                                    6.494363e+01
## D.ratio.sum.TfIdf.nwrds                                     7.135951e+00
## D.TfIdf.sum.stem.stop.Ratio                                 6.720453e+00
## color.fctrBlack                                             5.495087e+00
## D.ratio.nstopwrds.nwrds                                     5.004874e+00
## storage.fctr64                                              4.740955e+00
## D.TfIdf.sum.post.stop                                       4.719678e+00
## D.TfIdf.sum.post.stem                                       4.522182e+00
## color.fctrWhite                                             4.187605e+00
## D.sum.TfIdf                                                 4.149523e+00
## prdline.my.fctriPad 1                                       4.143156e+00
## color.fctrSpace Gray                                        4.062558e+00
## condition.fctrNew                                           3.987112e+00
## prdline.my.fctriPadAir                                      3.986975e+00
## storage.fctr16                                              3.937344e+00
## cellular.fctr1                                              3.714775e+00
## D.nuppr.log                                                 3.588109e+00
## D.nchrs.log                                                 3.447178e+00
## prdline.my.fctriPad 3+                                      3.442736e+00
## prdline.my.fctriPadmini                                     3.336970e+00
## cellular.fctrUnknown                                        3.200631e+00
## carrier.fctrAT&T                                            3.092894e+00
## D.nstopwrds.log                                             3.090547e+00
## D.nwrds.log                                                 3.090401e+00
## carrier.fctrUnknown                                         3.084186e+00
## carrier.fctrVerizon                                         2.796045e+00
## storage.fctrUnknown                                         2.673223e+00
## storage.fctr32                                              2.612027e+00
## condition.fctrNew other (see details)                       2.464048e+00
## D.terms.n.post.stop                                         2.304155e+00
## condition.fctrSeller refurbished                            2.231088e+00
## D.terms.n.post.stem                                         2.216511e+00
## D.npnct11.log                                               2.167865e+00
## prdline.my.fctriPadmini 2+                                  1.978667e+00
## prdline.my.fctriPad 2                                       1.961556e+00
## D.nwrds.unq.log                                             1.946190e+00
## D.npnct13.log                                               1.726664e+00
## condition.fctrFor parts or not working                      1.555871e+00
## color.fctrGold                                              1.455401e+00
## condition.fctrManufacturer refurbished                      1.388559e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                     1.157005e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                      1.112577e+00
## D.terms.n.stem.stop.Ratio                                   1.019426e+00
## D.ndgts.log                                                 9.524259e-01
## carrier.fctrSprint                                          9.491356e-01
## D.npnct15.log                                               8.008516e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                      7.351143e-01
## prdline.my.fctriPad 2:.clusterid.fctr4                      7.158364e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                     6.913168e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                     6.529339e-01
## carrier.fctrT-Mobile                                        6.218184e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                 6.151461e-01
## D.npnct05.log                                               5.705584e-01
## D.npnct08.log                                               5.581608e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                    5.403950e-01
## D.npnct14.log                                               5.369901e-01
## prdline.my.fctriPadmini:.clusterid.fctr3                    4.758212e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                     4.646786e-01
## D.npnct10.log                                               4.455789e-01
## D.npnct01.log                                               4.160190e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                      4.141739e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                     3.997075e-01
## D.npnct24.log                                               3.864426e-01
## D.npnct12.log                                               3.781070e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                      3.420448e-01
## D.npnct16.log                                               3.238302e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                 3.106229e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                     2.918164e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                     2.847269e-01
## carrier.fctrOther                                           2.227022e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                      2.014511e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                    1.790224e-01
## D.npnct03.log                                               1.256671e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                      1.214698e-01
## D.npnct06.log                                               1.005843e-01
## D.npnct28.log                                               2.723279e-03
## D.npnct09.log                                               2.450951e-03
## prdline.my.fctrUnknown:.clusterid.fctr4                     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4                     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                 0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadmini:.clusterid.fctr5                    0.000000e+00
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
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 40
```

![](ebayipads_txtclstr_files/figure-html/fit.models_2-5.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_2-6.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_2-7.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_2-8.png) ![](ebayipads_txtclstr_files/figure-html/fit.models_2-9.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 66      10066         N                                    0.172
## 951     10951         N                                    0.024
## 285     10285         Y                                    0.988
## 409     10409         N                                    0.508
## 1309    11309         N                                    0.520
## 602     10602         N                                    0.554
## 851     10851         N                                    0.570
## 1836    11836         N                                    0.606
## 282     10282         N                                    0.618
## 127     10127         N                                    0.644
## 1526    11526         N                                    0.648
## 512     10512         N                                    0.654
## 1700    11700         N                                    0.810
## 1769    11769         N                                    0.864
## 199     10199         N                                    0.946
##      sold.fctr.predict.All.X.no.rnorm.rf
## 66                                     N
## 951                                    N
## 285                                    Y
## 409                                    Y
## 1309                                   Y
## 602                                    Y
## 851                                    Y
## 1836                                   Y
## 282                                    Y
## 127                                    Y
## 1526                                   Y
## 512                                    Y
## 1700                                   Y
## 1769                                   Y
## 199                                    Y
##      sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 66                                           TRUE
## 951                                          TRUE
## 285                                          TRUE
## 409                                         FALSE
## 1309                                        FALSE
## 602                                         FALSE
## 851                                         FALSE
## 1836                                        FALSE
## 282                                         FALSE
## 127                                         FALSE
## 1526                                        FALSE
## 512                                         FALSE
## 1700                                        FALSE
## 1769                                        FALSE
## 199                                         FALSE
##      sold.fctr.predict.All.X.no.rnorm.rf.error .label
## 66                                       0.000  10066
## 951                                      0.000  10951
## 285                                      0.000  10285
## 409                                      0.008  10409
## 1309                                     0.020  11309
## 602                                      0.054  10602
## 851                                      0.070  10851
## 1836                                     0.106  11836
## 282                                      0.118  10282
## 127                                      0.144  10127
## 1526                                     0.148  11526
## 512                                      0.154  10512
## 1700                                     0.310  11700
## 1769                                     0.364  11769
## 199                                      0.446  10199
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 1359    11359         Y                                    0.006
## 1697    11697         Y                                    0.016
## 962     10962         Y                                    0.034
## 1605    11605         Y                                    0.038
## 846     10846         Y                                    0.038
## 1300    11300         Y                                    0.040
##      sold.fctr.predict.All.X.no.rnorm.rf
## 1359                                   N
## 1697                                   N
## 962                                    N
## 1605                                   N
## 846                                    N
## 1300                                   N
##      sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 1359                                        FALSE
## 1697                                        FALSE
## 962                                         FALSE
## 1605                                        FALSE
## 846                                         FALSE
## 1300                                        FALSE
##      sold.fctr.predict.All.X.no.rnorm.rf.error
## 1359                                    -0.494
## 1697                                    -0.484
## 962                                     -0.466
## 1605                                    -0.462
## 846                                     -0.462
## 1300                                    -0.460
##      UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 1705    11705         Y                                    0.108
## 131     10131         Y                                    0.172
## 394     10394         Y                                    0.222
## 117     10117         Y                                    0.248
## 882     10882         Y                                    0.362
## 423     10423         N                                    0.654
##      sold.fctr.predict.All.X.no.rnorm.rf
## 1705                                   N
## 131                                    N
## 394                                    N
## 117                                    N
## 882                                    N
## 423                                    Y
##      sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 1705                                        FALSE
## 131                                         FALSE
## 394                                         FALSE
## 117                                         FALSE
## 882                                         FALSE
## 423                                         FALSE
##      sold.fctr.predict.All.X.no.rnorm.rf.error
## 1705                                    -0.392
## 131                                     -0.328
## 394                                     -0.278
## 117                                     -0.252
## 882                                     -0.138
## 423                                      0.154
##     UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 581    10581         N                                    0.948
## 413    10413         N                                    0.966
## 526    10526         N                                    0.968
## 491    10491         N                                    0.984
## 488    10488         N                                    0.986
## 283    10283         N                                    0.994
##     sold.fctr.predict.All.X.no.rnorm.rf
## 581                                   Y
## 413                                   Y
## 526                                   Y
## 491                                   Y
## 488                                   Y
## 283                                   Y
##     sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 581                                        FALSE
## 413                                        FALSE
## 526                                        FALSE
## 491                                        FALSE
## 488                                        FALSE
## 283                                        FALSE
##     sold.fctr.predict.All.X.no.rnorm.rf.error
## 581                                     0.448
## 413                                     0.466
## 526                                     0.468
## 491                                     0.484
## 488                                     0.486
## 283                                     0.494
```

![](ebayipads_txtclstr_files/figure-html/fit.models_2-10.png) 

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
## 12 fit.models          7          2 166.156 182.46  16.304
## 13 fit.models          7          3 182.461     NA      NA
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

![](ebayipads_txtclstr_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 182.461 186.864   4.403
## 14 fit.data.training          8          0 186.864      NA      NA
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
## [1] "fitting model: Final.rf"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_txtclstr_files/figure-html/fit.data.training_0-1.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted       1861   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           3722   matrix     numeric  
## oob.times       1861   -none-     numeric  
## classes            2   -none-     character
## importance        88   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               1861   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            88   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtclstr_files/figure-html/fit.data.training_0-2.png) 

```
##    threshold   f.score
## 1        0.0 0.6321205
## 2        0.1 0.8233605
## 3        0.2 0.9398907
## 4        0.3 0.9839817
## 5        0.4 0.9976798
## 6        0.5 1.0000000
## 7        0.6 0.9976690
## 8        0.7 0.9415385
## 9        0.8 0.8563830
## 10       0.9 0.7661406
## 11       1.0 0.1181619
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Final.rf.N sold.fctr.predict.Final.rf.Y
## 1         N                         1001                           NA
## 2         Y                           NA                          860
##          Prediction
## Reference    N    Y
##         N 1001    0
##         Y    0  860
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      1.0000000      1.0000000      0.9980198      1.0000000      0.5378829 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](ebayipads_txtclstr_files/figure-html/fit.data.training_0-3.png) 

```
##   model_id model_method
## 1 Final.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     19.616                11.242
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.8113925
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.9980198                     1     0.6170003
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.002709495     0.005780847
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 186.864 210.326  23.463
## 15 fit.data.training          8          1 210.327      NA      NA
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
##                                             All.X.no.rnorm.rf.importance
## startprice.log                                              1.000000e+02
## biddable                                                    7.999853e+01
## idseq.my                                                    6.494363e+01
## D.ratio.sum.TfIdf.nwrds                                     7.135951e+00
## D.TfIdf.sum.stem.stop.Ratio                                 6.720453e+00
## prdline.my.fctriPadAir                                      3.986975e+00
## prdline.my.fctriPad 1                                       4.143156e+00
## D.TfIdf.sum.post.stop                                       4.719678e+00
## color.fctrWhite                                             4.187605e+00
## D.ratio.nstopwrds.nwrds                                     5.004874e+00
## cellular.fctr1                                              3.714775e+00
## D.TfIdf.sum.post.stem                                       4.522182e+00
## D.sum.TfIdf                                                 4.149523e+00
## color.fctrBlack                                             5.495087e+00
## prdline.my.fctriPad 3+                                      3.442736e+00
## carrier.fctrUnknown                                         3.084186e+00
## D.nstopwrds.log                                             3.090547e+00
## storage.fctr16                                              3.937344e+00
## D.nuppr.log                                                 3.588109e+00
## condition.fctrNew                                           3.987112e+00
## storage.fctr64                                              4.740955e+00
## D.nchrs.log                                                 3.447178e+00
## color.fctrSpace Gray                                        4.062558e+00
## prdline.my.fctriPadmini                                     3.336970e+00
## prdline.my.fctriPad 2                                       1.961556e+00
## D.nwrds.log                                                 3.090401e+00
## prdline.my.fctriPadmini 2+                                  1.978667e+00
## storage.fctr32                                              2.612027e+00
## cellular.fctrUnknown                                        3.200631e+00
## storage.fctrUnknown                                         2.673223e+00
## carrier.fctrVerizon                                         2.796045e+00
## D.terms.n.post.stem                                         2.216511e+00
## D.nwrds.unq.log                                             1.946190e+00
## D.terms.n.post.stop                                         2.304155e+00
## condition.fctrNew other (see details)                       2.464048e+00
## carrier.fctrAT&T                                            3.092894e+00
## condition.fctrSeller refurbished                            2.231088e+00
## D.npnct11.log                                               2.167865e+00
## condition.fctrFor parts or not working                      1.555871e+00
## D.npnct13.log                                               1.726664e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                      1.112577e+00
## condition.fctrManufacturer refurbished                      1.388559e+00
## color.fctrGold                                              1.455401e+00
## D.npnct15.log                                               8.008516e-01
## D.ndgts.log                                                 9.524259e-01
## carrier.fctrSprint                                          9.491356e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                    5.403950e-01
## prdline.my.fctrUnknown:.clusterid.fctr2                     1.157005e+00
## carrier.fctrT-Mobile                                        6.218184e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                     4.646786e-01
## prdline.my.fctriPad 2:.clusterid.fctr4                      7.158364e-01
## D.terms.n.stem.stop.Ratio                                   1.019426e+00
## D.npnct16.log                                               3.238302e-01
## D.npnct08.log                                               5.581608e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                     6.913168e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                     2.918164e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                     3.997075e-01
## prdline.my.fctriPadmini:.clusterid.fctr3                    4.758212e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                     6.529339e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                      4.141739e-01
## D.npnct05.log                                               5.705584e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                      7.351143e-01
## D.npnct14.log                                               5.369901e-01
## D.npnct24.log                                               3.864426e-01
## D.npnct06.log                                               1.005843e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                 3.106229e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                      3.420448e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                 6.151461e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                    1.790224e-01
## carrier.fctrOther                                           2.227022e-01
## D.npnct01.log                                               4.160190e-01
## D.npnct10.log                                               4.455789e-01
## D.npnct12.log                                               3.781070e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                      2.014511e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                     2.847269e-01
## D.npnct03.log                                               1.256671e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                      1.214698e-01
## D.npnct09.log                                               2.450951e-03
## D.npnct28.log                                               2.723279e-03
## prdline.my.fctrUnknown:.clusterid.fctr4                     0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4                     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                 0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                 0.000000e+00
## prdline.my.fctriPadmini:.clusterid.fctr5                    0.000000e+00
##                                              importance
## startprice.log                              100.0000000
## biddable                                     96.4936004
## idseq.my                                     65.8860060
## D.ratio.sum.TfIdf.nwrds                       6.2085107
## D.TfIdf.sum.stem.stop.Ratio                   6.0132393
## prdline.my.fctriPadAir                        5.0090219
## prdline.my.fctriPad 1                         4.8529413
## D.TfIdf.sum.post.stop                         4.6906771
## color.fctrWhite                               4.5580008
## D.ratio.nstopwrds.nwrds                       4.5228086
## cellular.fctr1                                4.3450784
## D.TfIdf.sum.post.stem                         4.1293066
## D.sum.TfIdf                                   3.9696778
## color.fctrBlack                               3.9287693
## prdline.my.fctriPad 3+                        3.8865164
## carrier.fctrUnknown                           3.8789174
## D.nstopwrds.log                               3.8406576
## storage.fctr16                                3.7872525
## D.nuppr.log                                   3.7565795
## condition.fctrNew                             3.7363752
## storage.fctr64                                3.7050475
## D.nchrs.log                                   3.6614697
## color.fctrSpace Gray                          3.5339614
## prdline.my.fctriPadmini                       3.2909269
## prdline.my.fctriPad 2                         3.2839118
## D.nwrds.log                                   3.1592663
## prdline.my.fctriPadmini 2+                    3.0333772
## storage.fctr32                                2.7392410
## cellular.fctrUnknown                          2.6237565
## storage.fctrUnknown                           2.5120680
## carrier.fctrVerizon                           2.3757116
## D.terms.n.post.stem                           2.2922990
## D.nwrds.unq.log                               2.2827367
## D.terms.n.post.stop                           2.2178682
## condition.fctrNew other (see details)         2.1743357
## carrier.fctrAT&T                              2.1397312
## condition.fctrSeller refurbished              2.1107241
## D.npnct11.log                                 2.0844559
## condition.fctrFor parts or not working        1.9557987
## D.npnct13.log                                 1.7056509
## prdline.my.fctriPad 2:.clusterid.fctr5        1.5040330
## condition.fctrManufacturer refurbished        1.4646742
## color.fctrGold                                1.4060567
## D.npnct15.log                                 1.2517800
## D.ndgts.log                                   1.0682712
## carrier.fctrSprint                            0.9466161
## prdline.my.fctriPadmini:.clusterid.fctr4      0.9436300
## prdline.my.fctrUnknown:.clusterid.fctr2       0.8558149
## carrier.fctrT-Mobile                          0.8188213
## prdline.my.fctriPad 3+:.clusterid.fctr3       0.7212765
## prdline.my.fctriPad 2:.clusterid.fctr4        0.7077766
## D.terms.n.stem.stop.Ratio                     0.6899287
## D.npnct16.log                                 0.6383372
## D.npnct08.log                                 0.6299951
## prdline.my.fctriPad 3+:.clusterid.fctr2       0.6299876
## prdline.my.fctriPad 3+:.clusterid.fctr4       0.5512301
## prdline.my.fctrUnknown:.clusterid.fctr3       0.5508152
## prdline.my.fctriPadmini:.clusterid.fctr3      0.5506909
## prdline.my.fctriPadAir:.clusterid.fctr2       0.5289309
## prdline.my.fctriPad 1:.clusterid.fctr2        0.5236141
## D.npnct05.log                                 0.4477161
## prdline.my.fctriPad 1:.clusterid.fctr4        0.4158463
## D.npnct14.log                                 0.4099003
## D.npnct24.log                                 0.4066333
## D.npnct06.log                                 0.3860126
## prdline.my.fctriPadmini 2+:.clusterid.fctr2   0.3840724
## prdline.my.fctriPad 2:.clusterid.fctr2        0.3533293
## prdline.my.fctriPadmini 2+:.clusterid.fctr3   0.3065203
## prdline.my.fctriPadmini:.clusterid.fctr2      0.2940868
## carrier.fctrOther                             0.2810304
## D.npnct01.log                                 0.2747522
## D.npnct10.log                                 0.2697887
## D.npnct12.log                                 0.2677574
## prdline.my.fctriPad 1:.clusterid.fctr3        0.2657201
## prdline.my.fctriPadAir:.clusterid.fctr3       0.2422099
## D.npnct03.log                                 0.2060545
## prdline.my.fctriPad 2:.clusterid.fctr3        0.1726246
## D.npnct09.log                                 0.0000000
## D.npnct28.log                                 0.0000000
## prdline.my.fctrUnknown:.clusterid.fctr4       0.0000000
## prdline.my.fctrUnknown:.clusterid.fctr5       0.0000000
## prdline.my.fctriPad 1:.clusterid.fctr5        0.0000000
## prdline.my.fctriPad 3+:.clusterid.fctr5       0.0000000
## prdline.my.fctriPadAir:.clusterid.fctr4       0.0000000
## prdline.my.fctriPadAir:.clusterid.fctr5       0.0000000
## prdline.my.fctriPadmini 2+:.clusterid.fctr4   0.0000000
## prdline.my.fctriPadmini 2+:.clusterid.fctr5   0.0000000
## prdline.my.fctriPadmini:.clusterid.fctr5      0.0000000
##                                             Final.rf.importance
## startprice.log                                      100.0000000
## biddable                                             96.4936004
## idseq.my                                             65.8860060
## D.ratio.sum.TfIdf.nwrds                               6.2085107
## D.TfIdf.sum.stem.stop.Ratio                           6.0132393
## prdline.my.fctriPadAir                                5.0090219
## prdline.my.fctriPad 1                                 4.8529413
## D.TfIdf.sum.post.stop                                 4.6906771
## color.fctrWhite                                       4.5580008
## D.ratio.nstopwrds.nwrds                               4.5228086
## cellular.fctr1                                        4.3450784
## D.TfIdf.sum.post.stem                                 4.1293066
## D.sum.TfIdf                                           3.9696778
## color.fctrBlack                                       3.9287693
## prdline.my.fctriPad 3+                                3.8865164
## carrier.fctrUnknown                                   3.8789174
## D.nstopwrds.log                                       3.8406576
## storage.fctr16                                        3.7872525
## D.nuppr.log                                           3.7565795
## condition.fctrNew                                     3.7363752
## storage.fctr64                                        3.7050475
## D.nchrs.log                                           3.6614697
## color.fctrSpace Gray                                  3.5339614
## prdline.my.fctriPadmini                               3.2909269
## prdline.my.fctriPad 2                                 3.2839118
## D.nwrds.log                                           3.1592663
## prdline.my.fctriPadmini 2+                            3.0333772
## storage.fctr32                                        2.7392410
## cellular.fctrUnknown                                  2.6237565
## storage.fctrUnknown                                   2.5120680
## carrier.fctrVerizon                                   2.3757116
## D.terms.n.post.stem                                   2.2922990
## D.nwrds.unq.log                                       2.2827367
## D.terms.n.post.stop                                   2.2178682
## condition.fctrNew other (see details)                 2.1743357
## carrier.fctrAT&T                                      2.1397312
## condition.fctrSeller refurbished                      2.1107241
## D.npnct11.log                                         2.0844559
## condition.fctrFor parts or not working                1.9557987
## D.npnct13.log                                         1.7056509
## prdline.my.fctriPad 2:.clusterid.fctr5                1.5040330
## condition.fctrManufacturer refurbished                1.4646742
## color.fctrGold                                        1.4060567
## D.npnct15.log                                         1.2517800
## D.ndgts.log                                           1.0682712
## carrier.fctrSprint                                    0.9466161
## prdline.my.fctriPadmini:.clusterid.fctr4              0.9436300
## prdline.my.fctrUnknown:.clusterid.fctr2               0.8558149
## carrier.fctrT-Mobile                                  0.8188213
## prdline.my.fctriPad 3+:.clusterid.fctr3               0.7212765
## prdline.my.fctriPad 2:.clusterid.fctr4                0.7077766
## D.terms.n.stem.stop.Ratio                             0.6899287
## D.npnct16.log                                         0.6383372
## D.npnct08.log                                         0.6299951
## prdline.my.fctriPad 3+:.clusterid.fctr2               0.6299876
## prdline.my.fctriPad 3+:.clusterid.fctr4               0.5512301
## prdline.my.fctrUnknown:.clusterid.fctr3               0.5508152
## prdline.my.fctriPadmini:.clusterid.fctr3              0.5506909
## prdline.my.fctriPadAir:.clusterid.fctr2               0.5289309
## prdline.my.fctriPad 1:.clusterid.fctr2                0.5236141
## D.npnct05.log                                         0.4477161
## prdline.my.fctriPad 1:.clusterid.fctr4                0.4158463
## D.npnct14.log                                         0.4099003
## D.npnct24.log                                         0.4066333
## D.npnct06.log                                         0.3860126
## prdline.my.fctriPadmini 2+:.clusterid.fctr2           0.3840724
## prdline.my.fctriPad 2:.clusterid.fctr2                0.3533293
## prdline.my.fctriPadmini 2+:.clusterid.fctr3           0.3065203
## prdline.my.fctriPadmini:.clusterid.fctr2              0.2940868
## carrier.fctrOther                                     0.2810304
## D.npnct01.log                                         0.2747522
## D.npnct10.log                                         0.2697887
## D.npnct12.log                                         0.2677574
## prdline.my.fctriPad 1:.clusterid.fctr3                0.2657201
## prdline.my.fctriPadAir:.clusterid.fctr3               0.2422099
## D.npnct03.log                                         0.2060545
## prdline.my.fctriPad 2:.clusterid.fctr3                0.1726246
## D.npnct09.log                                         0.0000000
## D.npnct28.log                                         0.0000000
## prdline.my.fctrUnknown:.clusterid.fctr4               0.0000000
## prdline.my.fctrUnknown:.clusterid.fctr5               0.0000000
## prdline.my.fctriPad 1:.clusterid.fctr5                0.0000000
## prdline.my.fctriPad 3+:.clusterid.fctr5               0.0000000
## prdline.my.fctriPadAir:.clusterid.fctr4               0.0000000
## prdline.my.fctriPadAir:.clusterid.fctr5               0.0000000
## prdline.my.fctriPadmini 2+:.clusterid.fctr4           0.0000000
## prdline.my.fctriPadmini 2+:.clusterid.fctr5           0.0000000
## prdline.my.fctriPadmini:.clusterid.fctr5              0.0000000
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

![](ebayipads_txtclstr_files/figure-html/fit.data.training_0-4.png) ![](ebayipads_txtclstr_files/figure-html/fit.data.training_0-5.png) ![](ebayipads_txtclstr_files/figure-html/fit.data.training_0-6.png) ![](ebayipads_txtclstr_files/figure-html/fit.data.training_0-7.png) ![](ebayipads_txtclstr_files/figure-html/fit.data.training_0-8.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1       10001         N                           0.244
## 2       10002         Y                           0.998
## 91      10091         Y                           0.998
## 1397    11397         N                           0.004
##      sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## 1                             N                                TRUE
## 2                             Y                                TRUE
## 91                            Y                                TRUE
## 1397                          N                                TRUE
##      sold.fctr.predict.Final.rf.error .label
## 1                                   0  10001
## 2                                   0  10002
## 91                                  0  10091
## 1397                                0  11397
## [1] "Inaccurate: "
## [1] UniqueID                            sold.fctr                          
## [3] sold.fctr.predict.Final.rf.prob     sold.fctr.predict.Final.rf         
## [5] sold.fctr.predict.Final.rf.accurate sold.fctr.predict.Final.rf.error   
## <0 rows> (or 0-length row.names)
```

![](ebayipads_txtclstr_files/figure-html/fit.data.training_0-9.png) 

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

![](ebayipads_txtclstr_files/figure-html/fit.data.training_0-10.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 210.327 216.101   5.774
## 16  predict.data.new          9          0 216.101      NA      NA
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

![](ebayipads_txtclstr_files/figure-html/predict.data.new-1.png) 

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

![](ebayipads_txtclstr_files/figure-html/predict.data.new-2.png) 

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

![](ebayipads_txtclstr_files/figure-html/predict.data.new-3.png) 

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

![](ebayipads_txtclstr_files/figure-html/predict.data.new-4.png) 

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

![](ebayipads_txtclstr_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1862    11862      <NA>                           0.256
## 1865    11865      <NA>                           0.520
## 1891    11891      <NA>                           0.846
## 2625    12625      <NA>                           0.440
##      sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## 1862                          N                                  NA
## 1865                          Y                                  NA
## 1891                          Y                                  NA
## 2625                          N                                  NA
##      sold.fctr.predict.Final.rf.error .label
## 1862                                0  11862
## 1865                                0  11865
## 1891                                0  11891
## 2625                                0  12625
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
## NA.27        NA      <NA>                              NA
## NA.207       NA      <NA>                              NA
## NA.243       NA      <NA>                              NA
## NA.645       NA      <NA>                              NA
## NA.669       NA      <NA>                              NA
## NA.676       NA      <NA>                              NA
##        sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## NA.27                        <NA>                                  NA
## NA.207                       <NA>                                  NA
## NA.243                       <NA>                                  NA
## NA.645                       <NA>                                  NA
## NA.669                       <NA>                                  NA
## NA.676                       <NA>                                  NA
##        sold.fctr.predict.Final.rf.error
## NA.27                                NA
## NA.207                               NA
## NA.243                               NA
## NA.645                               NA
## NA.669                               NA
## NA.676                               NA
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

![](ebayipads_txtclstr_files/figure-html/predict.data.new-6.png) 

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
## condit  207.4213  condit  494 134
## use     146.0063     use  290 614
## scratch 127.9519 scratch  286 502
## new     125.4260     new  156 388
## good    120.8098    good  197 262
## screen  106.8461  screen  213 503
##                    TfIdf            term freq pos
## bottom          7.515733          bottom    8  90
## addit           4.049016           addit    4  30
## spent           2.635344           spent    2 551
## invisibleshield 2.305926 invisibleshield    2 307
## fade            1.625238            fade    1 221
## caus            1.422084            caus    1 114
##           TfIdf  term freq pos
## final 1.1376668 final    1 230
## fold  1.1376668  fold    1 238
## goe   1.1376668   goe    1 260
## high  1.1376668  high    1 282
## hole  1.1376668  hole    1 284
## 79in  0.9480557  79in    1  16
##     UniqueID
## 520    10520
##                                                                                             descr.my
## 520 Apple iPad mini 1st Generation 16GB, Wi- Fi, 7.9in - Space Gray, great condition comes with the 
## [1] 181
## [1] "    Top_n post_stem_words TfIDf terms for descr.my:"
##              TfIdf      term freq pos top_n
## paperwork 1.422084 paperwork    1 421  TRUE
## working   1.264074   working    1 638  TRUE
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
## [1] 976  68
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 13         All.X.no.rnorm.rf        0.8135593   0.8532596     0.6221736
## 12      All.X.no.rnorm.rpart        0.8056497   0.8087593     0.6052550
## 5            Max.cor.Y.rpart        0.7954802   0.8157399     0.5865393
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7920904   0.8186805     0.5788853
## 11              All.X.glmnet        0.7887006   0.8437519     0.5729811
## 7    Interact.High.cor.Y.glm        0.7887006   0.8383252     0.5745991
## 10            All.X.bayesglm        0.7887006   0.8381377     0.5728335
## 6              Max.cor.Y.glm        0.7875706   0.8432177     0.5726922
## 8              Low.cor.X.glm        0.7841808   0.8422572     0.5633942
## 9                  All.X.glm        0.7841808   0.8336946     0.5636962
## 1          MFO.myMFO_classfr        0.5378531   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.5378531   0.5000000     0.0000000
## 2    Random.myrandom_classfr        0.4621469   0.5214656     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 13          NA                    0.5
## 12          NA                    0.6
## 5           NA                    0.5
## 4           NA                    0.6
## 11          NA                    0.5
## 7     961.7029                    0.4
## 10   1005.3201                    0.5
## 6     962.8176                    0.4
## 8     968.2921                    0.5
## 9     976.6092                    0.5
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
## [1] "All.X.no.rnorm.rf OOB confusion matrix & accuracy: "
##          Prediction
## Reference   N   Y
##         N 415  61
##         Y 104 305
##    prdline.my .n.OOB .n.Tst .freqRatio.Tst .freqRatio.OOB
## 3      iPad 2    171    154      0.1929825      0.1932203
## 4     iPad 3+    136    123      0.1541353      0.1536723
## 5     iPadAir    152    137      0.1716792      0.1717514
## 1     Unknown     97     87      0.1090226      0.1096045
## 6    iPadmini    126    114      0.1428571      0.1423729
## 2      iPad 1     99     89      0.1115288      0.1118644
## 7 iPadmini 2+    104     94      0.1177945      0.1175141
##   accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 3                 33               138        0.8070175
## 4                 31               105        0.7720588
## 5                 25               127        0.8355263
## 1                 20                77        0.7938144
## 6                 20               106        0.8412698
## 2                 18                81        0.8181818
## 7                 18                86        0.8269231
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
## startprice.log                                              1.000000e+02
## biddable                                                    7.999853e+01
## idseq.my                                                    6.494363e+01
## D.ratio.sum.TfIdf.nwrds                                     7.135951e+00
## D.TfIdf.sum.stem.stop.Ratio                                 6.720453e+00
## color.fctrBlack                                             5.495087e+00
## D.ratio.nstopwrds.nwrds                                     5.004874e+00
## storage.fctr64                                              4.740955e+00
## D.TfIdf.sum.post.stop                                       4.719678e+00
## D.TfIdf.sum.post.stem                                       4.522182e+00
## color.fctrWhite                                             4.187605e+00
## D.sum.TfIdf                                                 4.149523e+00
## prdline.my.fctriPad 1                                       4.143156e+00
## color.fctrSpace Gray                                        4.062558e+00
## condition.fctrNew                                           3.987112e+00
## prdline.my.fctriPadAir                                      3.986975e+00
## storage.fctr16                                              3.937344e+00
## cellular.fctr1                                              3.714775e+00
## D.nuppr.log                                                 3.588109e+00
## D.nchrs.log                                                 3.447178e+00
## prdline.my.fctriPad 3+                                      3.442736e+00
## prdline.my.fctriPadmini                                     3.336970e+00
## cellular.fctrUnknown                                        3.200631e+00
## carrier.fctrAT&T                                            3.092894e+00
## D.nstopwrds.log                                             3.090547e+00
## D.nwrds.log                                                 3.090401e+00
## carrier.fctrUnknown                                         3.084186e+00
## carrier.fctrVerizon                                         2.796045e+00
## storage.fctrUnknown                                         2.673223e+00
## storage.fctr32                                              2.612027e+00
## condition.fctrNew other (see details)                       2.464048e+00
## D.terms.n.post.stop                                         2.304155e+00
## condition.fctrSeller refurbished                            2.231088e+00
## D.terms.n.post.stem                                         2.216511e+00
## D.npnct11.log                                               2.167865e+00
## prdline.my.fctriPadmini 2+                                  1.978667e+00
## prdline.my.fctriPad 2                                       1.961556e+00
## D.nwrds.unq.log                                             1.946190e+00
## D.npnct13.log                                               1.726664e+00
## condition.fctrFor parts or not working                      1.555871e+00
## color.fctrGold                                              1.455401e+00
## condition.fctrManufacturer refurbished                      1.388559e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                     1.157005e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                      1.112577e+00
## D.terms.n.stem.stop.Ratio                                   1.019426e+00
## D.ndgts.log                                                 9.524259e-01
## carrier.fctrSprint                                          9.491356e-01
## D.npnct15.log                                               8.008516e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                      7.351143e-01
## prdline.my.fctriPad 2:.clusterid.fctr4                      7.158364e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                     6.913168e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                     6.529339e-01
## carrier.fctrT-Mobile                                        6.218184e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                 6.151461e-01
## D.npnct05.log                                               5.705584e-01
## D.npnct08.log                                               5.581608e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                    5.403950e-01
## D.npnct14.log                                               5.369901e-01
## prdline.my.fctriPadmini:.clusterid.fctr3                    4.758212e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                     4.646786e-01
## D.npnct10.log                                               4.455789e-01
## D.npnct01.log                                               4.160190e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                      4.141739e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                     3.997075e-01
## D.npnct24.log                                               3.864426e-01
## D.npnct12.log                                               3.781070e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                      3.420448e-01
## D.npnct16.log                                               3.238302e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                 3.106229e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                     2.918164e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                     2.847269e-01
## carrier.fctrOther                                           2.227022e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                      2.014511e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                    1.790224e-01
## D.npnct03.log                                               1.256671e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                      1.214698e-01
## D.npnct06.log                                               1.005843e-01
## D.npnct28.log                                               2.723279e-03
## D.npnct09.log                                               2.450951e-03
## prdline.my.fctrUnknown:.clusterid.fctr4                     0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4                     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                 0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                 0.000000e+00
## prdline.my.fctriPadmini:.clusterid.fctr5                    0.000000e+00
##                                              importance
## startprice.log                              100.0000000
## biddable                                     96.4936004
## idseq.my                                     65.8860060
## D.ratio.sum.TfIdf.nwrds                       6.2085107
## D.TfIdf.sum.stem.stop.Ratio                   6.0132393
## color.fctrBlack                               3.9287693
## D.ratio.nstopwrds.nwrds                       4.5228086
## storage.fctr64                                3.7050475
## D.TfIdf.sum.post.stop                         4.6906771
## D.TfIdf.sum.post.stem                         4.1293066
## color.fctrWhite                               4.5580008
## D.sum.TfIdf                                   3.9696778
## prdline.my.fctriPad 1                         4.8529413
## color.fctrSpace Gray                          3.5339614
## condition.fctrNew                             3.7363752
## prdline.my.fctriPadAir                        5.0090219
## storage.fctr16                                3.7872525
## cellular.fctr1                                4.3450784
## D.nuppr.log                                   3.7565795
## D.nchrs.log                                   3.6614697
## prdline.my.fctriPad 3+                        3.8865164
## prdline.my.fctriPadmini                       3.2909269
## cellular.fctrUnknown                          2.6237565
## carrier.fctrAT&T                              2.1397312
## D.nstopwrds.log                               3.8406576
## D.nwrds.log                                   3.1592663
## carrier.fctrUnknown                           3.8789174
## carrier.fctrVerizon                           2.3757116
## storage.fctrUnknown                           2.5120680
## storage.fctr32                                2.7392410
## condition.fctrNew other (see details)         2.1743357
## D.terms.n.post.stop                           2.2178682
## condition.fctrSeller refurbished              2.1107241
## D.terms.n.post.stem                           2.2922990
## D.npnct11.log                                 2.0844559
## prdline.my.fctriPadmini 2+                    3.0333772
## prdline.my.fctriPad 2                         3.2839118
## D.nwrds.unq.log                               2.2827367
## D.npnct13.log                                 1.7056509
## condition.fctrFor parts or not working        1.9557987
## color.fctrGold                                1.4060567
## condition.fctrManufacturer refurbished        1.4646742
## prdline.my.fctrUnknown:.clusterid.fctr2       0.8558149
## prdline.my.fctriPad 2:.clusterid.fctr5        1.5040330
## D.terms.n.stem.stop.Ratio                     0.6899287
## D.ndgts.log                                   1.0682712
## carrier.fctrSprint                            0.9466161
## D.npnct15.log                                 1.2517800
## prdline.my.fctriPad 1:.clusterid.fctr4        0.4158463
## prdline.my.fctriPad 2:.clusterid.fctr4        0.7077766
## prdline.my.fctriPad 3+:.clusterid.fctr2       0.6299876
## prdline.my.fctriPadAir:.clusterid.fctr2       0.5289309
## carrier.fctrT-Mobile                          0.8188213
## prdline.my.fctriPadmini 2+:.clusterid.fctr3   0.3065203
## D.npnct05.log                                 0.4477161
## D.npnct08.log                                 0.6299951
## prdline.my.fctriPadmini:.clusterid.fctr4      0.9436300
## D.npnct14.log                                 0.4099003
## prdline.my.fctriPadmini:.clusterid.fctr3      0.5506909
## prdline.my.fctriPad 3+:.clusterid.fctr3       0.7212765
## D.npnct10.log                                 0.2697887
## D.npnct01.log                                 0.2747522
## prdline.my.fctriPad 1:.clusterid.fctr2        0.5236141
## prdline.my.fctrUnknown:.clusterid.fctr3       0.5508152
## D.npnct24.log                                 0.4066333
## D.npnct12.log                                 0.2677574
## prdline.my.fctriPad 2:.clusterid.fctr2        0.3533293
## D.npnct16.log                                 0.6383372
## prdline.my.fctriPadmini 2+:.clusterid.fctr2   0.3840724
## prdline.my.fctriPad 3+:.clusterid.fctr4       0.5512301
## prdline.my.fctriPadAir:.clusterid.fctr3       0.2422099
## carrier.fctrOther                             0.2810304
## prdline.my.fctriPad 1:.clusterid.fctr3        0.2657201
## prdline.my.fctriPadmini:.clusterid.fctr2      0.2940868
## D.npnct03.log                                 0.2060545
## prdline.my.fctriPad 2:.clusterid.fctr3        0.1726246
## D.npnct06.log                                 0.3860126
## D.npnct28.log                                 0.0000000
## D.npnct09.log                                 0.0000000
## prdline.my.fctrUnknown:.clusterid.fctr4       0.0000000
## prdline.my.fctrUnknown:.clusterid.fctr5       0.0000000
## prdline.my.fctriPad 1:.clusterid.fctr5        0.0000000
## prdline.my.fctriPad 3+:.clusterid.fctr5       0.0000000
## prdline.my.fctriPadAir:.clusterid.fctr4       0.0000000
## prdline.my.fctriPadAir:.clusterid.fctr5       0.0000000
## prdline.my.fctriPadmini 2+:.clusterid.fctr4   0.0000000
## prdline.my.fctriPadmini 2+:.clusterid.fctr5   0.0000000
## prdline.my.fctriPadmini:.clusterid.fctr5      0.0000000
##                                             Final.rf.importance
## startprice.log                                      100.0000000
## biddable                                             96.4936004
## idseq.my                                             65.8860060
## D.ratio.sum.TfIdf.nwrds                               6.2085107
## D.TfIdf.sum.stem.stop.Ratio                           6.0132393
## color.fctrBlack                                       3.9287693
## D.ratio.nstopwrds.nwrds                               4.5228086
## storage.fctr64                                        3.7050475
## D.TfIdf.sum.post.stop                                 4.6906771
## D.TfIdf.sum.post.stem                                 4.1293066
## color.fctrWhite                                       4.5580008
## D.sum.TfIdf                                           3.9696778
## prdline.my.fctriPad 1                                 4.8529413
## color.fctrSpace Gray                                  3.5339614
## condition.fctrNew                                     3.7363752
## prdline.my.fctriPadAir                                5.0090219
## storage.fctr16                                        3.7872525
## cellular.fctr1                                        4.3450784
## D.nuppr.log                                           3.7565795
## D.nchrs.log                                           3.6614697
## prdline.my.fctriPad 3+                                3.8865164
## prdline.my.fctriPadmini                               3.2909269
## cellular.fctrUnknown                                  2.6237565
## carrier.fctrAT&T                                      2.1397312
## D.nstopwrds.log                                       3.8406576
## D.nwrds.log                                           3.1592663
## carrier.fctrUnknown                                   3.8789174
## carrier.fctrVerizon                                   2.3757116
## storage.fctrUnknown                                   2.5120680
## storage.fctr32                                        2.7392410
## condition.fctrNew other (see details)                 2.1743357
## D.terms.n.post.stop                                   2.2178682
## condition.fctrSeller refurbished                      2.1107241
## D.terms.n.post.stem                                   2.2922990
## D.npnct11.log                                         2.0844559
## prdline.my.fctriPadmini 2+                            3.0333772
## prdline.my.fctriPad 2                                 3.2839118
## D.nwrds.unq.log                                       2.2827367
## D.npnct13.log                                         1.7056509
## condition.fctrFor parts or not working                1.9557987
## color.fctrGold                                        1.4060567
## condition.fctrManufacturer refurbished                1.4646742
## prdline.my.fctrUnknown:.clusterid.fctr2               0.8558149
## prdline.my.fctriPad 2:.clusterid.fctr5                1.5040330
## D.terms.n.stem.stop.Ratio                             0.6899287
## D.ndgts.log                                           1.0682712
## carrier.fctrSprint                                    0.9466161
## D.npnct15.log                                         1.2517800
## prdline.my.fctriPad 1:.clusterid.fctr4                0.4158463
## prdline.my.fctriPad 2:.clusterid.fctr4                0.7077766
## prdline.my.fctriPad 3+:.clusterid.fctr2               0.6299876
## prdline.my.fctriPadAir:.clusterid.fctr2               0.5289309
## carrier.fctrT-Mobile                                  0.8188213
## prdline.my.fctriPadmini 2+:.clusterid.fctr3           0.3065203
## D.npnct05.log                                         0.4477161
## D.npnct08.log                                         0.6299951
## prdline.my.fctriPadmini:.clusterid.fctr4              0.9436300
## D.npnct14.log                                         0.4099003
## prdline.my.fctriPadmini:.clusterid.fctr3              0.5506909
## prdline.my.fctriPad 3+:.clusterid.fctr3               0.7212765
## D.npnct10.log                                         0.2697887
## D.npnct01.log                                         0.2747522
## prdline.my.fctriPad 1:.clusterid.fctr2                0.5236141
## prdline.my.fctrUnknown:.clusterid.fctr3               0.5508152
## D.npnct24.log                                         0.4066333
## D.npnct12.log                                         0.2677574
## prdline.my.fctriPad 2:.clusterid.fctr2                0.3533293
## D.npnct16.log                                         0.6383372
## prdline.my.fctriPadmini 2+:.clusterid.fctr2           0.3840724
## prdline.my.fctriPad 3+:.clusterid.fctr4               0.5512301
## prdline.my.fctriPadAir:.clusterid.fctr3               0.2422099
## carrier.fctrOther                                     0.2810304
## prdline.my.fctriPad 1:.clusterid.fctr3                0.2657201
## prdline.my.fctriPadmini:.clusterid.fctr2              0.2940868
## D.npnct03.log                                         0.2060545
## prdline.my.fctriPad 2:.clusterid.fctr3                0.1726246
## D.npnct06.log                                         0.3860126
## D.npnct28.log                                         0.0000000
## D.npnct09.log                                         0.0000000
## prdline.my.fctrUnknown:.clusterid.fctr4               0.0000000
## prdline.my.fctrUnknown:.clusterid.fctr5               0.0000000
## prdline.my.fctriPad 1:.clusterid.fctr5                0.0000000
## prdline.my.fctriPad 3+:.clusterid.fctr5               0.0000000
## prdline.my.fctriPadAir:.clusterid.fctr4               0.0000000
## prdline.my.fctriPadAir:.clusterid.fctr5               0.0000000
## prdline.my.fctriPadmini 2+:.clusterid.fctr4           0.0000000
## prdline.my.fctriPadmini 2+:.clusterid.fctr5           0.0000000
## prdline.my.fctriPadmini:.clusterid.fctr5              0.0000000
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

![](ebayipads_txtclstr_files/figure-html/predict.data.new-7.png) 

```r
if (glb_is_classification)
    print(table(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)]))
```

```
## 
##   N   Y 
## 537 261
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
## 16     predict.data.new          9          0 216.101 222.212   6.111
## 17 display.session.info         10          0 222.212      NA      NA
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
## 5         extract.features          3          0  20.371  79.628  59.257
## 11              fit.models          7          1 115.609 166.155  50.546
## 10              fit.models          7          0  88.733 115.609  26.876
## 14       fit.data.training          8          0 186.864 210.326  23.463
## 12              fit.models          7          2 166.156 182.460  16.304
## 16        predict.data.new          9          0 216.101 222.212   6.111
## 2             inspect.data          2          0  12.643  18.606   5.964
## 15       fit.data.training          8          1 210.327 216.101   5.774
## 13              fit.models          7          3 182.461 186.864   4.403
## 8          select.features          5          0  84.236  87.843   3.607
## 7      manage.missing.data          4          1  80.783  84.236   3.453
## 3               scrub.data          2          1  18.607  19.850   1.243
## 6             cluster.data          4          0  79.629  80.782   1.154
## 9  partition.data.training          6          0  87.843  88.732   0.889
## 4           transform.data          2          2  19.850  20.371   0.521
## 1              import.data          1          0  12.165  12.642   0.478
##    duration
## 5    59.257
## 11   50.546
## 10   26.876
## 14   23.462
## 12   16.304
## 16    6.111
## 2     5.963
## 15    5.774
## 13    4.403
## 8     3.607
## 7     3.453
## 3     1.243
## 6     1.153
## 9     0.889
## 4     0.521
## 1     0.477
## [1] "Total Elapsed Time: 222.212 secs"
```

![](ebayipads_txtclstr_files/figure-html/display.session.info-1.png) 

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
##  [1] gdata_2.17.0        randomForest_4.6-10 glmnet_2.0-2       
##  [4] arm_1.8-6           lme4_1.1-8          Matrix_1.2-2       
##  [7] MASS_7.3-43         rpart.plot_1.5.2    rpart_4.1-10       
## [10] ROCR_1.0-7          gplots_2.17.0       sampling_2.7       
## [13] tidyr_0.2.0         entropy_1.2.1       dynamicTreeCut_1.62
## [16] proxy_0.4-15        tm_0.6-2            NLP_0.1-8          
## [19] stringr_1.0.0       dplyr_0.4.2         plyr_1.8.3         
## [22] sqldf_0.4-10        RSQLite_1.0.0       DBI_0.3.1          
## [25] gsubfn_0.6-6        proto_0.3-10        reshape2_1.4.1     
## [28] doMC_1.3.3          iterators_1.0.7     foreach_1.4.2      
## [31] doBy_4.5-13         survival_2.38-3     caret_6.0-52       
## [34] ggplot2_1.0.1       lattice_0.20-33    
## 
## loaded via a namespace (and not attached):
##  [1] splines_3.2.1       gtools_3.5.0        assertthat_0.1     
##  [4] stats4_3.2.1        yaml_2.1.13         slam_0.1-32        
##  [7] quantreg_5.11       chron_2.3-47        digest_0.6.8       
## [10] RColorBrewer_1.1-2  minqa_1.2.4         colorspace_1.2-6   
## [13] htmltools_0.2.6     lpSolve_5.6.11      BradleyTerry2_1.0-6
## [16] SparseM_1.6         scales_0.2.5        brglm_0.5-9        
## [19] mgcv_1.8-6          car_2.0-25          nnet_7.3-10        
## [22] lazyeval_0.1.10     pbkrtest_0.4-2      magrittr_1.5       
## [25] evaluate_0.7        nlme_3.1-121        class_7.3-13       
## [28] tools_3.2.1         formatR_1.2         munsell_0.4.2      
## [31] compiler_3.2.1      e1071_1.6-6         caTools_1.17.1     
## [34] nloptr_1.0.4        bitops_1.0-6        labeling_0.3       
## [37] rmarkdown_0.7       gtable_0.1.2        codetools_0.2-14   
## [40] abind_1.4-3         R6_2.1.0            knitr_1.10.5       
## [43] KernSmooth_2.23-15  stringi_0.5-5       Rcpp_0.11.6        
## [46] coda_0.17-1
```
