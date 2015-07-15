# eBay:iPads:: sold classification:: txtterms
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
glb_out_pfx <- "txtterms_"
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
        mod_raw <- gsub(" NEW\\!(SCREEN|ONE) ", " NEW\\! \\1 ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" new looking$", " looks new", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" newer ", " new ", mod_raw, ignore.case=TRUE);                
        mod_raw <- gsub(" opening", " opened", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" operated", " operational", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" performance", " performs", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" personalized ", " personal ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" Keeped ", " Kept ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" knicks ", " nicks ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("^READiPad ", "READ iPad ", mod_raw, ignore.case=TRUE);   
        mod_raw <- gsub(" REFURB\\.", "  REFURBISHED\\.", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" reponding", " respond", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" rotation ", " rotate ", mod_raw, ignore.case=TRUE);   
        mod_raw <- gsub("^somescratches ", "some scratches ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" scratchs ", " scratches ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" SCREEB ", " SCREEN ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" shipped ", " ship ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" sides ", " side ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" skinned,", " skin,", mod_raw, ignore.case=TRUE);        
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
                                        ,"changed","chrome","contents","control","cream"
    ,"decent","defender","disclaimer","distressed","divider"
    ,"dlxnqat9g5wt","done","dont","dust","duty"
                                        ,"erased","ereader","exact"
                                        ,"film","flickers"
                                        ,"generic","genuine","glitter"
                                        ,"hdmi","higher","hospital"
                                        ,"impact","instead"
                                        ,"jack","july"
                                        ,"keeps"
                            ,"letters","limited","lining","liquid","local","looping","loss"
                    ,"mb292ll","mc707ll","mc916ll","md789ll","mf432ll","mgye2ll"
                    ,"mind","mixed"
                                        ,"neither","november"
                                        ,"occasional"
                                        ,"pet","plug","poor","provided"
                                        ,"recently","red","result","running"
    ,"seem","semi","serious","shell","short","size","slice","smoke"
    ,"softer","software","somewhat","soon"
    ,"sparingly","sparkiling","special","speck","speed"
    ,"stains","standup","status","stopped","subtle","sustained","swappacom"
                                    ,"technical","therefore","though","totally","touchy"
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

glb_cluster <- FALSE # or TRUE

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

![](ebayipads_txtterms_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor  bgn end elapsed
## 1 import.data          1          0 8.32  NA      NA
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
##          label step_major step_minor   bgn   end elapsed
## 1  import.data          1          0 8.320 8.701   0.381
## 2 inspect.data          2          0 8.702    NA      NA
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

![](ebayipads_txtterms_files/figure-html/inspect.data-1.png) 

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

![](ebayipads_txtterms_files/figure-html/inspect.data-2.png) 

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

![](ebayipads_txtterms_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: .rnorm"
```

![](ebayipads_txtterms_files/figure-html/inspect.data-4.png) 

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
## 2 inspect.data          2          0  8.702 12.646   3.944
## 3   scrub.data          2          1 12.647     NA      NA
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
                        c(glb_id_var, glb_rsp_var, glb_category_vars, glb_txt_vars, cols),
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
##            label step_major step_minor    bgn    end elapsed
## 3     scrub.data          2          1 12.647 13.347     0.7
## 4 transform.data          2          2 13.348     NA      NA
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
hex_vctr <- c("\211", "\235", "\317", "\333")
for (row_pos in c(1904, 2157)) {
    tmp_str <- unlist(strsplit(glb_allobs_df[row_pos, "descr.my"], ""))
    glb_allobs_df[row_pos, "descr.my"] <- paste0(tmp_str[!tmp_str %in% hex_vctr],
                                                         collapse="")
}
```

## Step `2.2: transform data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 4   transform.data          2          2 13.348 13.792   0.444
## 5 extract.features          3          0 13.792     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor  bgn end elapsed
## 1 extract.features_bgn          1          0 13.8  NA      NA
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
## 1                extract.features_bgn          1          0 13.800 13.815
## 2 extract.features_factorize.str.vars          2          0 13.816     NA
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
    for (txt_var in glb_txt_vars) {
        print(sprintf("    Top_n TfIDf terms for %s:", txt_var))
        # This impacts stemming probably due to lazy parameter
        print(myprint_df(full_TfIdf_df <- get_corpus_terms(glb_corpus_lst[[txt_var]]), 
                   glb_top_n[[txt_var]]))
        glb_post_stop_words_terms_df_lst[[txt_var]] <- full_TfIdf_df
        TfIdf_mtrx <- as.matrix(DocumentTermMatrix(glb_corpus_lst[[txt_var]], 
                                        control=list(weighting=weightTfIdf)))
        rownames(TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        glb_post_stop_words_TfIdf_mtrx_lst[[txt_var]] <- TfIdf_mtrx
        
        tmp_allobs_df <- glb_allobs_df[, c(glb_id_var, glb_rsp_var)]
        tmp_allobs_df$terms.n.post.stop <- rowSums(TfIdf_mtrx > 0)
        tmp_allobs_df$TfIdf.sum.post.stop <- rowSums(TfIdf_mtrx)        
        
        glb_corpus_lst[[txt_var]] <- tm_map(glb_corpus_lst[[txt_var]], stemDocument,
                                            "english", lazy=TRUE) #Features ???
        print(myprint_df(full_TfIdf_df <- get_corpus_terms(glb_corpus_lst[[txt_var]]), 
                   glb_top_n[[txt_var]]))
        TfIdf_stem_mtrx <- as.matrix(DocumentTermMatrix(glb_corpus_lst[[txt_var]], 
                                        control=list(weighting=weightTfIdf)))
        rownames(TfIdf_stem_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
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
## 2 extract.features_factorize.str.vars          2          0 13.816 14.077
## 3       extract.features_process.text          3          0 14.077     NA
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
## 3          0 14.077 15.744   1.667
## 4          1 15.744     NA      NA
## [1] "Remaining compound terms in descr.my: "
##                                                    label step_major
## 4 extract.features_process.text_reporting_compound_terms          3
## 5                          extract.features_build.corpus          4
##   step_minor    bgn    end elapsed
## 4          1 15.744 15.749   0.005
## 5          0 15.749     NA      NA
## [1] "Building glb_corpus_lst..."
## [1] "    Top_n TfIDf terms for descr.my:"
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
## [1] "Rows: 816; Cols: 4"
##              TfIdf      term freq pos
## condition 207.6392 condition  497 163
## new       125.1790       new  156 484
## used      122.6714      used  237 781
## good      120.6369      good  197 321
## scratches 113.4465 scratches  254 641
## screen    105.8086    screen  210 643
##              TfIdf      term freq pos
## general   3.854852   general    4 312
## along     3.357156     along    3  44
## models    2.305926    models    2 468
## unsealed  2.190630  unsealed    2 769
## bright    1.981000    bright    2 107
## backwiped 1.137667 backwiped    1  75
##              TfIdf     term freq pos
## said     1.1376668     said    1 634
## sales    1.1376668    sales    1 636
## seconds  1.1376668  seconds    1 651
## setup    1.1376668    setup    1 661
## shipment 1.1376668 shipment    1 668
## 79in     0.9480557     79in    1  16
##              TfIdf     term freq pos
## said     1.1376668     said    1 634
## sales    1.1376668    sales    1 636
## seconds  1.1376668  seconds    1 651
## setup    1.1376668    setup    1 661
## shipment 1.1376668 shipment    1 668
## 79in     0.9480557     79in    1  16
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
## [1] "Rows: 681; Cols: 4"
##            TfIdf    term freq pos
## condit  207.4276  condit  492 137
## use     145.7789     use  288 650
## scratch 127.6927 scratch  286 533
## new     125.1790     new  156 404
## good    120.7263    good  197 270
## screen  106.6741  screen  213 534
##             TfIdf   term freq pos
## 100    34.1933619    100   64   1
## order  17.3376678  order   21 423
## squad   6.6652947  squad    5 588
## space   5.4921507  space    7 581
## rubber  1.6252383 rubber    1 527
## 79in    0.9480557   79in    1  16
##              TfIdf     term freq pos
## said     1.1376668     said    1 529
## second   1.1376668   second    1 540
## setup    1.1376668    setup    1 548
## shipment 1.1376668 shipment    1 554
## damaged  1.0342426  damaged    1 163
## 79in     0.9480557     79in    1  16
##              TfIdf     term freq pos
## said     1.1376668     said    1 529
## second   1.1376668   second    1 540
## setup    1.1376668    setup    1 548
## shipment 1.1376668 shipment    1 554
## damaged  1.0342426  damaged    1 163
## 79in     0.9480557     79in    1  16
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
## terms.n.post.stop         -0.082040646
## TfIdf.sum.post.stop       -0.032744659
## terms.n.post.stem         -0.081871577
## TfIdf.sum.post.stem       -0.034885895
## terms.n.stem.stop.Ratio    0.017153589
## TfIdf.sum.stem.stop.Ratio -0.002724955
##                           label step_major step_minor    bgn   end elapsed
## 5 extract.features_build.corpus          4          0 15.749 26.68  10.932
## 6  extract.features_extract.DTM          5          0 26.681    NA      NA
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
## 6 extract.features_extract.DTM          5          0 26.681 28.368   1.688
## 7  extract.features_report.DTM          6          0 28.369     NA      NA
## [1] "Reporting TfIDf terms for descr.my..."
## [1] "   Full TermMatrix:"
## <<DocumentTermMatrix (documents: 2659, terms: 681)>>
## Non-/sparse entries: 8345/1802434
## Sparsity           : 100%
## Maximal term length: 16
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
## [1] "   Sparse TermMatrix:"
## <<DocumentTermMatrix (documents: 2659, terms: 8)>>
## Non-/sparse entries: 2059/19213
## Sparsity           : 90%
## Maximal term length: 7
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
```

```
## Warning in myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full",
## colorcol_name = "in.sprs"): converting in.sprs to class:factor
```

![](ebayipads_txtterms_files/figure-html/extract.features-1.png) 

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](ebayipads_txtterms_files/figure-html/extract.features-2.png) ![](ebayipads_txtterms_files/figure-html/extract.features-3.png) 

```
## Warning in rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df,
## terms_TfIdf_df): object 'full_TfIdf_mtrx' not found
```

```
##                         label step_major step_minor    bgn    end elapsed
## 7 extract.features_report.DTM          6          0 28.369 30.499    2.13
## 8   extract.features_bind.DTM          7          0 30.499     NA      NA
## [1] "Binding DTM for descr.my..."
##                       label step_major step_minor    bgn  end elapsed
## 8 extract.features_bind.DTM          7          0 30.499 31.2   0.701
## 9 extract.features_bind.DXM          8          0 31.200   NA      NA
## [1] "Binding DXM for descr.my..."
```

```
## Warning in rm(log_X_df, txt_X_df): object 'log_X_df' not found
```

![](ebayipads_txtterms_files/figure-html/extract.features-4.png) 

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
##                        label step_major step_minor    bgn    end elapsed
## 9  extract.features_bind.DXM          8          0 31.200 63.478  32.279
## 10      extract.features_end          9          0 63.479     NA      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                                    label step_major
## 9                              extract.features_bind.DXM          8
## 5                          extract.features_build.corpus          4
## 7                            extract.features_report.DTM          6
## 6                           extract.features_extract.DTM          5
## 3                          extract.features_process.text          3
## 8                              extract.features_bind.DTM          7
## 2                    extract.features_factorize.str.vars          2
## 1                                   extract.features_bgn          1
## 4 extract.features_process.text_reporting_compound_terms          3
##   step_minor    bgn    end elapsed duration
## 9          0 31.200 63.478  32.279   32.278
## 5          0 15.749 26.680  10.932   10.931
## 7          0 28.369 30.499   2.130    2.130
## 6          0 26.681 28.368   1.688    1.687
## 3          0 14.077 15.744   1.667    1.667
## 8          0 30.499 31.200   0.701    0.701
## 2          0 13.816 14.077   0.261    0.261
## 1          0 13.800 13.815   0.015    0.015
## 4          1 15.744 15.749   0.005    0.005
## [1] "Total Elapsed Time: 63.478 secs"
```

![](ebayipads_txtterms_files/figure-html/extract.features-5.png) 

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

![](ebayipads_txtterms_files/figure-html/extract.features-6.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5 extract.features          3          0 13.792 64.848  51.056
## 6     cluster.data          4          0 64.848     NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 6        cluster.data          4          0 64.848 65.794   0.946
## 7 manage.missing.data          4          1 65.794     NA      NA
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
##                    1522                    1522                    2167 
##                 D.T.use             D.T.scratch                 D.T.new 
##                    2371                    2373                    2503 
##                D.T.good              D.T.screen                D.T.ipad 
##                    2462                    2446                    2427 
##               D.T.great                D.T.work               D.T.excel 
##                    2534                    2464                    2560 
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
##                    1522                    1522                    2167 
##                 D.T.use             D.T.scratch                 D.T.new 
##                    2371                    2373                    2503 
##                D.T.good              D.T.screen                D.T.ipad 
##                    2462                    2446                    2427 
##               D.T.great                D.T.work               D.T.excel 
##                    2534                    2464                    2560 
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
## 7 manage.missing.data          4          1 65.794 65.934   0.141
## 8     select.features          5          0 65.935     NA      NA
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
## D.terms.n.post.stop                 D.terms.n.post.stop -0.0820406460
## D.terms.n.post.stem                 D.terms.n.post.stem -0.0818715766
## D.npnct14.log                             D.npnct14.log -0.0784119354
## cellular.fctr                             cellular.fctr -0.0722383245
## carrier.fctr                               carrier.fctr -0.0677807650
## D.nwrds.unq.log                         D.nwrds.unq.log -0.0647054109
## D.ndgts.log                                 D.ndgts.log -0.0633901377
## D.npnct09.log                             D.npnct09.log -0.0617301891
## D.nwrds.log                                 D.nwrds.log -0.0591808768
## D.nchrs.log                                 D.nchrs.log -0.0566606213
## D.npnct12.log                             D.npnct12.log -0.0565901515
## D.nuppr.log                                 D.nuppr.log -0.0555211157
## D.ratio.nstopwrds.nwrds         D.ratio.nstopwrds.nwrds  0.0550700654
## D.npnct28.log                             D.npnct28.log -0.0523776347
## D.npnct06.log                             D.npnct06.log -0.0497583609
## D.nstopwrds.log                         D.nstopwrds.log -0.0472166865
## D.npnct24.log                             D.npnct24.log -0.0459503420
## D.npnct16.log                             D.npnct16.log -0.0447153098
## color.fctr                                   color.fctr -0.0426291775
## prdline.my.fctr                         prdline.my.fctr -0.0403911061
## D.npnct15.log                             D.npnct15.log  0.0400921803
## D.npnct08.log                             D.npnct08.log -0.0394718187
## D.T.new                                         D.T.new -0.0381164552
## D.T.condit                                   D.T.condit -0.0378848641
## D.npnct13.log                             D.npnct13.log -0.0367501751
## D.TfIdf.sum.post.stem             D.TfIdf.sum.post.stem -0.0348858951
## D.sum.TfIdf                                 D.sum.TfIdf -0.0348858951
## D.TfIdf.sum.post.stop             D.TfIdf.sum.post.stop -0.0327446592
## D.T.excel                                     D.T.excel  0.0267983603
## D.npnct03.log                             D.npnct03.log  0.0258721719
## D.npnct07.log                             D.npnct07.log  0.0250156240
## D.T.screen                                   D.T.screen  0.0241354666
## D.npnct10.log                             D.npnct10.log -0.0240301079
## .rnorm                                           .rnorm -0.0228561937
## D.npnct18.log                             D.npnct18.log -0.0214919447
## D.npnct11.log                             D.npnct11.log -0.0188179512
## D.terms.n.stem.stop.Ratio     D.terms.n.stem.stop.Ratio  0.0171535890
## D.T.ipad                                       D.T.ipad -0.0171141093
## D.T.work                                       D.T.work -0.0132365704
## D.T.use                                         D.T.use  0.0120945268
## D.P.mini                                       D.P.mini -0.0111321924
## storage.fctr                               storage.fctr -0.0103459049
## D.P.air                                         D.P.air -0.0091681483
## D.ratio.sum.TfIdf.nwrds         D.ratio.sum.TfIdf.nwrds  0.0082864649
## D.T.great                                     D.T.great  0.0078979293
## D.T.scratch                                 D.T.scratch -0.0073123675
## D.npnct01.log                             D.npnct01.log  0.0042941114
## D.TfIdf.sum.stem.stop.Ratio D.TfIdf.sum.stem.stop.Ratio -0.0027249551
## D.T.good                                       D.T.good -0.0004140426
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
## D.terms.n.post.stop                       0 0.0820406460
## D.terms.n.post.stem                       0 0.0818715766
## D.npnct14.log                             0 0.0784119354
## cellular.fctr                             0 0.0722383245
## carrier.fctr                              0 0.0677807650
## D.nwrds.unq.log                           0 0.0647054109
## D.ndgts.log                               0 0.0633901377
## D.npnct09.log                             0 0.0617301891
## D.nwrds.log                               0 0.0591808768
## D.nchrs.log                               0 0.0566606213
## D.npnct12.log                             0 0.0565901515
## D.nuppr.log                               0 0.0555211157
## D.ratio.nstopwrds.nwrds                   0 0.0550700654
## D.npnct28.log                             0 0.0523776347
## D.npnct06.log                             0 0.0497583609
## D.nstopwrds.log                           0 0.0472166865
## D.npnct24.log                             0 0.0459503420
## D.npnct16.log                             0 0.0447153098
## color.fctr                                0 0.0426291775
## prdline.my.fctr                           0 0.0403911061
## D.npnct15.log                             0 0.0400921803
## D.npnct08.log                             0 0.0394718187
## D.T.new                                   0 0.0381164552
## D.T.condit                                0 0.0378848641
## D.npnct13.log                             0 0.0367501751
## D.TfIdf.sum.post.stem                     0 0.0348858951
## D.sum.TfIdf                               0 0.0348858951
## D.TfIdf.sum.post.stop                     0 0.0327446592
## D.T.excel                                 0 0.0267983603
## D.npnct03.log                             0 0.0258721719
## D.npnct07.log                             0 0.0250156240
## D.T.screen                                0 0.0241354666
## D.npnct10.log                             0 0.0240301079
## .rnorm                                    0 0.0228561937
## D.npnct18.log                             0 0.0214919447
## D.npnct11.log                             0 0.0188179512
## D.terms.n.stem.stop.Ratio                 0 0.0171535890
## D.T.ipad                                  0 0.0171141093
## D.T.work                                  0 0.0132365704
## D.T.use                                   0 0.0120945268
## D.P.mini                                  0 0.0111321924
## storage.fctr                              0 0.0103459049
## D.P.air                                   0 0.0091681483
## D.ratio.sum.TfIdf.nwrds                   0 0.0082864649
## D.T.great                                 0 0.0078979293
## D.T.scratch                               0 0.0073123675
## D.npnct01.log                             0 0.0042941114
## D.TfIdf.sum.stem.stop.Ratio               0 0.0027249551
## D.T.good                                  0 0.0004140426
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
## D.P.http                                  0           NA
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## [1] "cor(D.TfIdf.sum.post.stem, D.sum.TfIdf)=1.0000"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0349"
## [1] "cor(sold.fctr, D.sum.TfIdf)=-0.0349"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.sum.TfIdf as highly correlated with
## D.TfIdf.sum.post.stem
```

```
## [1] "cor(D.nchrs.log, D.nuppr.log)=0.9995"
## [1] "cor(sold.fctr, D.nchrs.log)=-0.0567"
## [1] "cor(sold.fctr, D.nuppr.log)=-0.0555"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nuppr.log as highly correlated with
## D.nchrs.log
```

```
## [1] "cor(D.terms.n.post.stem, D.terms.n.post.stop)=0.9991"
## [1] "cor(sold.fctr, D.terms.n.post.stem)=-0.0819"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0820"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.terms.n.post.stem as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.TfIdf.sum.post.stem, D.TfIdf.sum.post.stop)=0.9974"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0349"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stop)=-0.0327"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stop as highly correlated with
## D.TfIdf.sum.post.stem
```

```
## [1] "cor(D.nchrs.log, D.nwrds.unq.log)=0.9935"
## [1] "cor(sold.fctr, D.nchrs.log)=-0.0567"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0647"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nchrs.log as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(D.nwrds.log, D.nwrds.unq.log)=0.9929"
## [1] "cor(sold.fctr, D.nwrds.log)=-0.0592"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0647"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.log as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(D.nwrds.unq.log, D.terms.n.post.stop)=0.9756"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0647"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0820"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.unq.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.npnct24.log, D.ratio.nstopwrds.nwrds)=-0.9654"
## [1] "cor(sold.fctr, D.npnct24.log)=-0.0460"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0551"
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
## [1] "cor(D.TfIdf.sum.post.stem, D.ratio.nstopwrds.nwrds)=-0.9270"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0349"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0551"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stem as highly correlated with
## D.ratio.nstopwrds.nwrds
```

```
## [1] "cor(D.nstopwrds.log, D.terms.n.post.stop)=0.8963"
## [1] "cor(sold.fctr, D.nstopwrds.log)=-0.0472"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0820"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nstopwrds.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.ratio.nstopwrds.nwrds, D.terms.n.post.stop)=-0.8687"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0551"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0820"
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
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0820"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.npnct13.log as highly correlated with
## D.terms.n.post.stop
```

```
##                             id         cor.y exclude.as.feat    cor.y.abs
## 68                        sold  1.0000000000               1 1.0000000000
## 61                    biddable  0.5485860292               0 0.5485860292
## 54     D.ratio.nstopwrds.nwrds  0.0550700654               0 0.0550700654
## 34               D.npnct15.log  0.0400921803               0 0.0400921803
## 6                    D.T.excel  0.0267983603               0 0.0267983603
## 22               D.npnct03.log  0.0258721719               0 0.0258721719
## 26               D.npnct07.log  0.0250156240               0 0.0250156240
## 12                  D.T.screen  0.0241354666               0 0.0241354666
## 59   D.terms.n.stem.stop.Ratio  0.0171535890               0 0.0171535890
## 13                     D.T.use  0.0120945268               0 0.0120945268
## 55     D.ratio.sum.TfIdf.nwrds  0.0082864649               0 0.0082864649
## 8                    D.T.great  0.0078979293               0 0.0078979293
## 20               D.npnct01.log  0.0042941114               0 0.0042941114
## 7                     D.T.good -0.0004140426               0 0.0004140426
## 17 D.TfIdf.sum.stem.stop.Ratio -0.0027249551               0 0.0027249551
## 11                 D.T.scratch -0.0073123675               0 0.0073123675
## 2                      D.P.air -0.0091681483               0 0.0091681483
## 71                storage.fctr -0.0103459049               0 0.0103459049
## 4                     D.P.mini -0.0111321924               0 0.0111321924
## 14                    D.T.work -0.0132365704               0 0.0132365704
## 9                     D.T.ipad -0.0171141093               0 0.0171141093
## 30               D.npnct11.log -0.0188179512               0 0.0188179512
## 37               D.npnct18.log -0.0214919447               0 0.0214919447
## 1                       .rnorm -0.0228561937               0 0.0228561937
## 29               D.npnct10.log -0.0240301079               0 0.0240301079
## 16       D.TfIdf.sum.post.stop -0.0327446592               0 0.0327446592
## 15       D.TfIdf.sum.post.stem -0.0348858951               0 0.0348858951
## 56                 D.sum.TfIdf -0.0348858951               0 0.0348858951
## 32               D.npnct13.log -0.0367501751               0 0.0367501751
## 5                   D.T.condit -0.0378848641               0 0.0378848641
## 10                     D.T.new -0.0381164552               0 0.0381164552
## 27               D.npnct08.log -0.0394718187               0 0.0394718187
## 67             prdline.my.fctr -0.0403911061               0 0.0403911061
## 64                  color.fctr -0.0426291775               0 0.0426291775
## 35               D.npnct16.log -0.0447153098               0 0.0447153098
## 43               D.npnct24.log -0.0459503420               0 0.0459503420
## 50             D.nstopwrds.log -0.0472166865               0 0.0472166865
## 25               D.npnct06.log -0.0497583609               0 0.0497583609
## 47               D.npnct28.log -0.0523776347               0 0.0523776347
## 51                 D.nuppr.log -0.0555211157               0 0.0555211157
## 31               D.npnct12.log -0.0565901515               0 0.0565901515
## 18                 D.nchrs.log -0.0566606213               0 0.0566606213
## 52                 D.nwrds.log -0.0591808768               0 0.0591808768
## 28               D.npnct09.log -0.0617301891               0 0.0617301891
## 19                 D.ndgts.log -0.0633901377               0 0.0633901377
## 53             D.nwrds.unq.log -0.0647054109               0 0.0647054109
## 62                carrier.fctr -0.0677807650               0 0.0677807650
## 63               cellular.fctr -0.0722383245               0 0.0722383245
## 33               D.npnct14.log -0.0784119354               0 0.0784119354
## 57         D.terms.n.post.stem -0.0818715766               0 0.0818715766
## 58         D.terms.n.post.stop -0.0820406460               0 0.0820406460
## 24               D.npnct05.log -0.1178427734               0 0.1178427734
## 65              condition.fctr -0.1531007887               0 0.1531007887
## 60                    UniqueID -0.1904241613               1 0.1904241613
## 66                    idseq.my -0.1904241613               0 0.1904241613
## 69                  startprice -0.4556701005               1 0.4556701005
## 70              startprice.log -0.4668597069               0 0.4668597069
## 3                     D.P.http            NA               0           NA
## 21               D.npnct02.log            NA               0           NA
## 23               D.npnct04.log            NA               0           NA
## 36               D.npnct17.log            NA               0           NA
## 38               D.npnct19.log            NA               0           NA
## 39               D.npnct20.log            NA               0           NA
## 40               D.npnct21.log            NA               0           NA
## 41               D.npnct22.log            NA               0           NA
## 42               D.npnct23.log            NA               0           NA
## 44               D.npnct25.log            NA               0           NA
## 45               D.npnct26.log            NA               0           NA
## 46               D.npnct27.log            NA               0           NA
## 48               D.npnct29.log            NA               0           NA
## 49               D.npnct30.log            NA               0           NA
##                 cor.high.X   freqRatio percentUnique zeroVar   nzv
## 68                    <NA>    1.163953    0.10746910   FALSE FALSE
## 61                    <NA>    1.223417    0.10746910   FALSE FALSE
## 54     D.terms.n.post.stop   12.903614    4.03009135   FALSE FALSE
## 34                    <NA>  153.500000    0.16120365   FALSE  TRUE
## 6                     <NA>  149.833333    0.75228372   FALSE  TRUE
## 22                    <NA>   83.318182    0.16120365   FALSE  TRUE
## 26                    <NA> 1860.000000    0.10746910   FALSE  TRUE
## 12                    <NA>   56.966667    0.80601827   FALSE  TRUE
## 59                    <NA>   77.913043    0.48361096   FALSE  TRUE
## 13                    <NA>   51.750000    0.91348737   FALSE  TRUE
## 55                    <NA>   63.058824   34.87372380   FALSE FALSE
## 8                     <NA>   98.777778    0.80601827   FALSE  TRUE
## 20                    <NA>   53.029412    0.32240731   FALSE  TRUE
## 7                     <NA>   49.257143    0.85975282   FALSE  TRUE
## 17                    <NA>   65.235294   32.93927996   FALSE FALSE
## 11                    <NA>   43.631579    0.85975282   FALSE  TRUE
## 2                     <NA>  123.000000    0.16120365   FALSE  TRUE
## 71                    <NA>    2.739003    0.26867276   FALSE FALSE
## 4                     <NA>   92.000000    0.16120365   FALSE  TRUE
## 14                    <NA>   68.880000    0.69854917   FALSE  TRUE
## 9                     <NA>   44.631579    0.85975282   FALSE  TRUE
## 30                    <NA>    9.385965    0.37614186   FALSE FALSE
## 37                    <NA> 1860.000000    0.10746910   FALSE  TRUE
## 1                     <NA>    1.000000  100.00000000   FALSE FALSE
## 29                    <NA>  309.000000    0.16120365   FALSE  TRUE
## 16   D.TfIdf.sum.post.stem   63.058824   34.39011284   FALSE FALSE
## 15 D.ratio.nstopwrds.nwrds   63.058824   34.33637829   FALSE FALSE
## 56   D.TfIdf.sum.post.stem   63.058824   34.33637829   FALSE FALSE
## 32     D.terms.n.post.stop    5.210728    0.32240731   FALSE FALSE
## 5                     <NA>   25.779661    0.91348737   FALSE  TRUE
## 10                    <NA>   92.263158    0.85975282   FALSE  TRUE
## 27                    <NA>   69.653846    0.21493821   FALSE  TRUE
## 67                    <NA>    1.135048    0.37614186   FALSE FALSE
## 64                    <NA>    1.576837    0.26867276   FALSE FALSE
## 35           D.npnct06.log   31.280702    0.16120365   FALSE  TRUE
## 43 D.ratio.nstopwrds.nwrds    1.355696    0.10746910   FALSE FALSE
## 50     D.terms.n.post.stop   13.764706    0.80601827   FALSE FALSE
## 25                    <NA>   33.773585    0.16120365   FALSE  TRUE
## 47                    <NA>  463.750000    0.16120365   FALSE  TRUE
## 51             D.nchrs.log   18.500000    4.40623321   FALSE FALSE
## 31                    <NA>   26.848485    0.21493821   FALSE  TRUE
## 18         D.nwrds.unq.log   15.750000    5.64212789   FALSE FALSE
## 52         D.nwrds.unq.log   12.903614    1.28962923   FALSE FALSE
## 28                    <NA>  308.666667    0.21493821   FALSE  TRUE
## 19                    <NA>   27.063492    0.69854917   FALSE  TRUE
## 53     D.terms.n.post.stop    8.246154    0.80601827   FALSE FALSE
## 62           cellular.fctr    3.192529    0.37614186   FALSE FALSE
## 63                    <NA>    2.120229    0.16120365   FALSE FALSE
## 33                    <NA>   35.372549    0.26867276   FALSE  TRUE
## 57     D.terms.n.post.stop    8.246154    0.80601827   FALSE FALSE
## 58                    <NA>    8.859504    0.80601827   FALSE FALSE
## 24                    <NA>   40.355556    0.10746910   FALSE  TRUE
## 65                    <NA>    4.006920    0.32240731   FALSE FALSE
## 60                    <NA>    1.000000  100.00000000   FALSE FALSE
## 66                    <NA>    1.000000  100.00000000   FALSE FALSE
## 69                    <NA>    2.807692   30.14508329   FALSE FALSE
## 70                    <NA>    2.807692   30.14508329   FALSE FALSE
## 3                     <NA>    0.000000    0.05373455    TRUE  TRUE
## 21                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 23                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 36                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 38                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 39                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 40                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 41                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 42                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 44                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 45                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 46                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 48                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 49                    <NA>    0.000000    0.05373455    TRUE  TRUE
##    myNearZV is.cor.y.abs.low
## 68    FALSE            FALSE
## 61    FALSE            FALSE
## 54    FALSE            FALSE
## 34    FALSE            FALSE
## 6     FALSE            FALSE
## 22    FALSE            FALSE
## 26     TRUE            FALSE
## 12    FALSE            FALSE
## 59    FALSE             TRUE
## 13    FALSE             TRUE
## 55    FALSE             TRUE
## 8     FALSE             TRUE
## 20    FALSE             TRUE
## 7     FALSE             TRUE
## 17    FALSE             TRUE
## 11    FALSE             TRUE
## 2     FALSE             TRUE
## 71    FALSE             TRUE
## 4     FALSE             TRUE
## 14    FALSE             TRUE
## 9     FALSE             TRUE
## 30    FALSE             TRUE
## 37     TRUE             TRUE
## 1     FALSE            FALSE
## 29    FALSE            FALSE
## 16    FALSE            FALSE
## 15    FALSE            FALSE
## 56    FALSE            FALSE
## 32    FALSE            FALSE
## 5     FALSE            FALSE
## 10    FALSE            FALSE
## 27    FALSE            FALSE
## 67    FALSE            FALSE
## 64    FALSE            FALSE
## 35    FALSE            FALSE
## 43    FALSE            FALSE
## 50    FALSE            FALSE
## 25    FALSE            FALSE
## 47    FALSE            FALSE
## 51    FALSE            FALSE
## 31    FALSE            FALSE
## 18    FALSE            FALSE
## 52    FALSE            FALSE
## 28    FALSE            FALSE
## 19    FALSE            FALSE
## 53    FALSE            FALSE
## 62    FALSE            FALSE
## 63    FALSE            FALSE
## 33    FALSE            FALSE
## 57    FALSE            FALSE
## 58    FALSE            FALSE
## 24    FALSE            FALSE
## 65    FALSE            FALSE
## 60    FALSE            FALSE
## 66    FALSE            FALSE
## 69    FALSE            FALSE
## 70    FALSE            FALSE
## 3      TRUE               NA
## 21     TRUE               NA
## 23     TRUE               NA
## 36     TRUE               NA
## 38     TRUE               NA
## 39     TRUE               NA
## 40     TRUE               NA
## 41     TRUE               NA
## 42     TRUE               NA
## 44     TRUE               NA
## 45     TRUE               NA
## 46     TRUE               NA
## 48     TRUE               NA
## 49     TRUE               NA
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

![](ebayipads_txtterms_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##               id       cor.y exclude.as.feat  cor.y.abs cor.high.X
## 26 D.npnct07.log  0.02501562               0 0.02501562       <NA>
## 37 D.npnct18.log -0.02149194               0 0.02149194       <NA>
## 3       D.P.http          NA               0         NA       <NA>
## 21 D.npnct02.log          NA               0         NA       <NA>
## 23 D.npnct04.log          NA               0         NA       <NA>
## 36 D.npnct17.log          NA               0         NA       <NA>
## 38 D.npnct19.log          NA               0         NA       <NA>
## 39 D.npnct20.log          NA               0         NA       <NA>
## 40 D.npnct21.log          NA               0         NA       <NA>
## 41 D.npnct22.log          NA               0         NA       <NA>
## 42 D.npnct23.log          NA               0         NA       <NA>
## 44 D.npnct25.log          NA               0         NA       <NA>
## 45 D.npnct26.log          NA               0         NA       <NA>
## 46 D.npnct27.log          NA               0         NA       <NA>
## 48 D.npnct29.log          NA               0         NA       <NA>
## 49 D.npnct30.log          NA               0         NA       <NA>
##    freqRatio percentUnique zeroVar  nzv myNearZV is.cor.y.abs.low
## 26      1860    0.10746910   FALSE TRUE     TRUE            FALSE
## 37      1860    0.10746910   FALSE TRUE     TRUE             TRUE
## 3          0    0.05373455    TRUE TRUE     TRUE               NA
## 21         0    0.05373455    TRUE TRUE     TRUE               NA
## 23         0    0.05373455    TRUE TRUE     TRUE               NA
## 36         0    0.05373455    TRUE TRUE     TRUE               NA
## 38         0    0.05373455    TRUE TRUE     TRUE               NA
## 39         0    0.05373455    TRUE TRUE     TRUE               NA
## 40         0    0.05373455    TRUE TRUE     TRUE               NA
## 41         0    0.05373455    TRUE TRUE     TRUE               NA
## 42         0    0.05373455    TRUE TRUE     TRUE               NA
## 44         0    0.05373455    TRUE TRUE     TRUE               NA
## 45         0    0.05373455    TRUE TRUE     TRUE               NA
## 46         0    0.05373455    TRUE TRUE     TRUE               NA
## 48         0    0.05373455    TRUE TRUE     TRUE               NA
## 49         0    0.05373455    TRUE TRUE     TRUE               NA
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
##                    1522                    1522                    2167 
##                 D.T.use             D.T.scratch                 D.T.new 
##                    2371                    2373                    2503 
##                D.T.good              D.T.screen                D.T.ipad 
##                    2462                    2446                    2427 
##               D.T.great                D.T.work               D.T.excel 
##                    2534                    2464                    2560 
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
##                     label step_major step_minor    bgn   end elapsed
## 8         select.features          5          0 65.935 67.44   1.505
## 9 partition.data.training          6          0 67.441    NA      NA
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
## [1] 71 12
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
## 68             sold  1.0000000            TRUE 1.0000000       <NA>
## 60         UniqueID -0.1904242            TRUE 0.1904242       <NA>
## sold.fctr sold.fctr         NA            TRUE        NA       <NA>
##           freqRatio percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## 68         1.163953     0.1074691   FALSE FALSE    FALSE            FALSE
## 60         1.000000   100.0000000   FALSE FALSE    FALSE            FALSE
## sold.fctr        NA            NA      NA    NA       NA               NA
##           interaction.feat rsp_var_raw id_var rsp_var
## 68                      NA        TRUE     NA      NA
## 60                      NA       FALSE   TRUE      NA
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
## [1] 2659   67
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 1861   66
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 976  66
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 885  66
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 798  66
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
##                      label step_major step_minor    bgn   end elapsed
## 9  partition.data.training          6          0 67.441 68.28   0.839
## 10              fit.models          7          0 68.280    NA      NA
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
## 1                      0.323                 0.003         0.5
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

![](ebayipads_txtterms_files/figure-html/fit.models_0-1.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-2.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-3.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-4.png) 

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
## 1                      0.259                 0.002   0.5143786
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

![](ebayipads_txtterms_files/figure-html/fit.models_0-5.png) 

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
## 1               0                      0.624                 0.011
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

![](ebayipads_txtterms_files/figure-html/fit.models_0-6.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-7.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-8.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-9.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-10.png) 

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
## 1               0                      0.464                 0.008
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

![](ebayipads_txtterms_files/figure-html/fit.models_0-11.png) ![](ebayipads_txtterms_files/figure-html/fit.models_0-12.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-13.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-14.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-15.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-16.png) 

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
## 1                      1.013                 0.012   0.8085735
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

![](ebayipads_txtterms_files/figure-html/fit.models_0-17.png) ![](ebayipads_txtterms_files/figure-html/fit.models_0-18.png) ![](ebayipads_txtterms_files/figure-html/fit.models_0-19.png) ![](ebayipads_txtterms_files/figure-html/fit.models_0-20.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-21.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-22.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-23.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-24.png) 

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
## 1                       0.94                 0.011   0.8349826
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

![](ebayipads_txtterms_files/figure-html/fit.models_0-25.png) ![](ebayipads_txtterms_files/figure-html/fit.models_0-26.png) ![](ebayipads_txtterms_files/figure-html/fit.models_0-27.png) ![](ebayipads_txtterms_files/figure-html/fit.models_0-28.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.0992  -0.7121  -0.5102   0.7391   2.1050  
## 
## Coefficients:
##                                    Estimate Std. Error z value Pr(>|z|)
## (Intercept)                         2.80667    0.49288   5.694 1.24e-08
## biddable                            3.17394    1.61551   1.965 0.049453
## startprice.log                     -0.75755    0.09129  -8.298  < 2e-16
## `biddable:D.terms.n.post.stop`      0.48909    0.32660   1.498 0.134253
## `biddable:D.TfIdf.sum.post.stem`    0.34883    0.21771   1.602 0.109093
## `biddable:D.ratio.nstopwrds.nwrds` -1.20995    1.60444  -0.754 0.450773
## `biddable:D.npnct06.log`           -0.25310    0.84377  -0.300 0.764208
## `biddable:D.nchrs.log`              0.58602    1.09548   0.535 0.592692
## `biddable:D.nwrds.unq.log`         -4.38399    3.56250  -1.231 0.218476
## `biddable:cellular.fctr1`          -0.23892    0.28879  -0.827 0.408050
## `biddable:cellular.fctrUnknown`    -1.28296    0.36910  -3.476 0.000509
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
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  939.83  on 965  degrees of freedom
## AIC: 961.83
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtterms_files/figure-html/fit.models_0-29.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6356589
## 3        0.2 0.7100694
## 4        0.3 0.7394439
## 5        0.4 0.7461024
## 6        0.5 0.7465116
## 7        0.6 0.7348952
## 8        0.7 0.6919946
## 9        0.8 0.5466035
## 10       0.9 0.3879004
## 11       1.0 0.0000000
```

![](ebayipads_txtterms_files/figure-html/fit.models_0-30.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_0-31.png) 

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
## 9        0.8 0.5655877
## 10       0.9 0.4423440
## 11       1.0 0.0000000
```

![](ebayipads_txtterms_files/figure-html/fit.models_0-32.png) 

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
## 1               1                      0.963                 0.015
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.839679                    0.5       0.7465116        0.7704987
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7491833              0.802415     0.5354264   0.8384587
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7699877        0.7887006
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7603001             0.8151638     0.5745991    961.8295
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.005998662      0.01291382
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
## [1] "    indep_vars: biddable, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_txtterms_files/figure-html/fit.models_0-33.png) ![](ebayipads_txtterms_files/figure-html/fit.models_0-34.png) ![](ebayipads_txtterms_files/figure-html/fit.models_0-35.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](ebayipads_txtterms_files/figure-html/fit.models_0-36.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.4271  -0.7165  -0.3083   0.6711   2.5888  
## 
## Coefficients:
##                                            Estimate Std. Error z value
## (Intercept)                              -5.382e-02  5.261e+00  -0.010
## biddable                                  1.478e+00  1.877e-01   7.876
## D.npnct15.log                             1.366e+00  7.556e-01   1.808
## D.T.excel                                 1.558e-01  4.397e-01   0.354
## D.npnct03.log                             8.826e-01  1.574e+00   0.561
## D.T.screen                               -5.518e-02  6.152e-01  -0.090
## D.terms.n.stem.stop.Ratio                 1.960e-01  4.602e+00   0.043
## D.T.use                                   8.838e-01  5.558e-01   1.590
## D.ratio.sum.TfIdf.nwrds                   2.653e-01  2.323e-01   1.142
## D.T.great                                 3.166e-01  5.813e-01   0.545
## D.npnct01.log                             2.352e-01  5.127e-01   0.459
## D.T.good                                  5.483e-01  6.329e-01   0.866
## D.TfIdf.sum.stem.stop.Ratio               3.218e+00  2.993e+00   1.075
## D.T.scratch                               1.170e+00  6.727e-01   1.740
## D.P.air                                   1.666e-01  1.066e+00   0.156
## storage.fctr16                            5.544e-01  5.021e-01   1.104
## storage.fctr32                            4.555e-01  5.274e-01   0.864
## storage.fctr64                            1.078e+00  5.159e-01   2.090
## storage.fctrUnknown                       6.445e-01  6.801e-01   0.948
## D.P.mini                                  2.298e-01  8.102e-01   0.284
## D.T.work                                 -2.843e-01  7.461e-01  -0.381
## D.T.ipad                                 -3.721e-01  7.321e-01  -0.508
## D.npnct11.log                             1.167e-01  3.177e-01   0.367
## .rnorm                                   -1.175e-01  8.908e-02  -1.319
## D.npnct10.log                             9.028e-01  1.251e+00   0.722
## D.T.condit                               -1.170e+00  6.802e-01  -1.720
## D.T.new                                  -3.155e-01  4.396e-01  -0.718
## D.npnct08.log                            -8.014e-01  7.048e-01  -1.137
## `prdline.my.fctriPad 1`                   4.966e-01  4.541e-01   1.093
## `prdline.my.fctriPad 2`                   7.061e-01  4.543e-01   1.554
## `prdline.my.fctriPad 3+`                  5.641e-01  4.370e-01   1.291
## prdline.my.fctriPadAir                    1.233e+00  4.513e-01   2.733
## prdline.my.fctriPadmini                   7.128e-01  4.300e-01   1.658
## `prdline.my.fctriPadmini 2+`              8.789e-01  4.764e-01   1.845
## color.fctrBlack                          -1.238e-01  2.475e-01  -0.500
## color.fctrGold                           -1.929e-01  4.908e-01  -0.393
## `color.fctrSpace Gray`                    1.357e-01  3.073e-01   0.442
## color.fctrWhite                           1.416e-01  2.389e-01   0.592
## D.npnct06.log                            -1.434e+00  1.087e+00  -1.319
## D.npnct28.log                            -3.043e+00  6.674e+02  -0.005
## D.npnct12.log                             1.167e-01  6.933e-01   0.168
## D.npnct09.log                            -7.648e+00  3.504e+02  -0.022
## D.ndgts.log                               3.472e-01  3.990e-01   0.870
## cellular.fctr1                            9.051e-02  2.047e-01   0.442
## cellular.fctrUnknown                     -2.866e-01  4.279e-01  -0.670
## D.npnct14.log                            -7.643e-01  7.284e-01  -1.049
## D.terms.n.post.stop                      -3.795e-02  3.651e-02  -1.039
## D.npnct05.log                            -2.819e+00  1.675e+00  -1.682
## `condition.fctrFor parts or not working` -3.555e-01  3.904e-01  -0.911
## `condition.fctrManufacturer refurbished` -1.963e-01  6.507e-01  -0.302
## condition.fctrNew                        -4.289e-01  2.936e-01  -1.461
## `condition.fctrNew other (see details)`  -2.950e-01  4.065e-01  -0.726
## `condition.fctrSeller refurbished`       -8.835e-01  4.464e-01  -1.979
## idseq.my                                 -5.767e-04  1.925e-04  -2.995
## startprice.log                           -9.601e-01  1.369e-01  -7.015
##                                          Pr(>|z|)    
## (Intercept)                               0.99184    
## biddable                                 3.38e-15 ***
## D.npnct15.log                             0.07054 .  
## D.T.excel                                 0.72303    
## D.npnct03.log                             0.57502    
## D.T.screen                                0.92852    
## D.terms.n.stem.stop.Ratio                 0.96603    
## D.T.use                                   0.11181    
## D.ratio.sum.TfIdf.nwrds                   0.25335    
## D.T.great                                 0.58600    
## D.npnct01.log                             0.64635    
## D.T.good                                  0.38634    
## D.TfIdf.sum.stem.stop.Ratio               0.28225    
## D.T.scratch                               0.08187 .  
## D.P.air                                   0.87583    
## storage.fctr16                            0.26959    
## storage.fctr32                            0.38774    
## storage.fctr64                            0.03662 *  
## storage.fctrUnknown                       0.34331    
## D.P.mini                                  0.77669    
## D.T.work                                  0.70314    
## D.T.ipad                                  0.61128    
## D.npnct11.log                             0.71350    
## .rnorm                                    0.18705    
## D.npnct10.log                             0.47035    
## D.T.condit                                0.08535 .  
## D.T.new                                   0.47284    
## D.npnct08.log                             0.25548    
## `prdline.my.fctriPad 1`                   0.27418    
## `prdline.my.fctriPad 2`                   0.12016    
## `prdline.my.fctriPad 3+`                  0.19679    
## prdline.my.fctriPadAir                    0.00629 ** 
## prdline.my.fctriPadmini                   0.09735 .  
## `prdline.my.fctriPadmini 2+`              0.06505 .  
## color.fctrBlack                           0.61702    
## color.fctrGold                            0.69430    
## `color.fctrSpace Gray`                    0.65880    
## color.fctrWhite                           0.55357    
## D.npnct06.log                             0.18729    
## D.npnct28.log                             0.99636    
## D.npnct12.log                             0.86629    
## D.npnct09.log                             0.98259    
## D.ndgts.log                               0.38426    
## cellular.fctr1                            0.65845    
## cellular.fctrUnknown                      0.50301    
## D.npnct14.log                             0.29401    
## D.terms.n.post.stop                       0.29858    
## D.npnct05.log                             0.09250 .  
## `condition.fctrFor parts or not working`  0.36244    
## `condition.fctrManufacturer refurbished`  0.76293    
## condition.fctrNew                         0.14409    
## `condition.fctrNew other (see details)`   0.46794    
## `condition.fctrSeller refurbished`        0.04781 *  
## idseq.my                                  0.00274 ** 
## startprice.log                           2.29e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  870.61  on 921  degrees of freedom
## AIC: 980.61
## 
## Number of Fisher Scoring iterations: 13
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtterms_files/figure-html/fit.models_0-37.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6748092
## 3        0.2 0.7169150
## 4        0.3 0.7586207
## 5        0.4 0.7720829
## 6        0.5 0.7808858
## 7        0.6 0.7652174
## 8        0.7 0.7231788
## 9        0.8 0.6176912
## 10       0.9 0.4236111
## 11       1.0 0.0000000
```

![](ebayipads_txtterms_files/figure-html/fit.models_0-38.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               453
## 2         Y                               116
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                72
## 2                               335
##          Prediction
## Reference   N   Y
##         N 453  72
##         Y 116 335
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.073770e-01   6.098441e-01   7.812112e-01   8.316731e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   1.309621e-69   1.712124e-03 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_txtterms_files/figure-html/fit.models_0-39.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6677995
## 3        0.2 0.7176471
## 4        0.3 0.7370737
## 5        0.4 0.7580453
## 6        0.5 0.7623888
## 7        0.6 0.7594595
## 8        0.7 0.7209302
## 9        0.8 0.6276423
## 10       0.9 0.4798535
## 11       1.0 0.0000000
```

![](ebayipads_txtterms_files/figure-html/fit.models_0-40.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               398
## 2         Y                               109
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                78
## 2                               300
##          Prediction
## Reference   N   Y
##         N 398  78
##         Y 109 300
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.887006e-01   5.726857e-01   7.603001e-01   8.151638e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.563801e-54   2.824855e-02 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       feats
## 1 biddable, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.176                 0.104
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8658642                    0.5       0.7808858         0.760236
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7812112             0.8316731     0.5157767   0.8449025
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7623888        0.7887006
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7603001             0.8151638     0.5726857    980.6074
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01365954      0.02754959
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 10 fit.models          7          0 68.280 93.261  24.981
## 11 fit.models          7          1 93.262     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor    bgn end elapsed
## 1 fit.models_1_bgn          1          0 97.402  NA      NA
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
##              label step_major step_minor    bgn    end elapsed
## 1 fit.models_1_bgn          1          0 97.402 97.423   0.021
## 2 fit.models_1_glm          2          0 97.423     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-1.png) ![](ebayipads_txtterms_files/figure-html/fit.models_1-2.png) ![](ebayipads_txtterms_files/figure-html/fit.models_1-3.png) 

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
## -3.6377  -0.6935  -0.2411   0.6476   2.5432  
## 
## Coefficients: (2 not defined because of singularities)
##                                            Estimate Std. Error z value
## (Intercept)                               7.849e+01  4.361e+01   1.800
## biddable                                  1.471e+00  1.927e-01   7.630
## D.ratio.nstopwrds.nwrds                  -1.693e+01  7.189e+00  -2.354
## D.npnct15.log                             7.303e-01  8.182e-01   0.893
## D.T.excel                                 4.722e-01  4.881e-01   0.967
## D.npnct03.log                             7.286e-01  1.863e+00   0.391
## D.T.screen                                1.693e-01  7.190e-01   0.235
## D.terms.n.stem.stop.Ratio                -6.612e+01  4.118e+01  -1.606
## D.T.use                                   1.391e+00  6.958e-01   1.999
## D.ratio.sum.TfIdf.nwrds                   7.753e-01  7.548e-01   1.027
## D.T.great                                 7.095e-01  6.225e-01   1.140
## D.npnct01.log                            -3.938e-01  6.716e-01  -0.586
## D.T.good                                  9.181e-01  7.018e-01   1.308
## D.TfIdf.sum.stem.stop.Ratio               8.504e+00  1.943e+01   0.438
## D.T.scratch                               1.802e+00  8.196e-01   2.199
## D.P.air                                   8.713e-01  1.248e+00   0.698
## storage.fctr16                            2.378e-01  5.194e-01   0.458
## storage.fctr32                            2.767e-02  5.461e-01   0.051
## storage.fctr64                            8.125e-01  5.308e-01   1.531
## storage.fctrUnknown                       2.494e-01  7.004e-01   0.356
## D.P.mini                                  1.954e-01  8.010e-01   0.244
## D.T.work                                  2.042e-01  7.993e-01   0.256
## D.T.ipad                                 -3.611e-01  8.250e-01  -0.438
## D.npnct11.log                            -5.341e-02  3.864e-01  -0.138
## .rnorm                                   -1.036e-01  9.124e-02  -1.136
## D.npnct10.log                             1.767e-02  1.515e+00   0.012
## D.TfIdf.sum.post.stop                     1.123e+00  2.959e+00   0.380
## D.TfIdf.sum.post.stem                    -7.680e-01  3.083e+00  -0.249
## D.sum.TfIdf                                      NA         NA      NA
## D.npnct13.log                            -4.918e-02  4.191e-01  -0.117
## D.T.condit                                4.162e-01  1.043e+00   0.399
## D.T.new                                  -1.722e-01  6.404e-01  -0.269
## D.npnct08.log                            -7.515e-01  7.768e-01  -0.967
## `prdline.my.fctriPad 1`                   4.138e-01  4.660e-01   0.888
## `prdline.my.fctriPad 2`                   6.081e-01  4.718e-01   1.289
## `prdline.my.fctriPad 3+`                  4.419e-01  4.515e-01   0.979
## prdline.my.fctriPadAir                    1.168e+00  4.639e-01   2.517
## prdline.my.fctriPadmini                   5.574e-01  4.439e-01   1.256
## `prdline.my.fctriPadmini 2+`              8.415e-01  4.932e-01   1.706
## color.fctrBlack                          -1.571e-01  2.554e-01  -0.615
## color.fctrGold                           -3.127e-01  5.010e-01  -0.624
## `color.fctrSpace Gray`                    1.696e-01  3.205e-01   0.529
## color.fctrWhite                           9.751e-02  2.469e-01   0.395
## D.npnct16.log                             1.012e+00  2.498e+00   0.405
## D.npnct24.log                            -1.282e+01  8.748e+00  -1.465
## D.nstopwrds.log                           4.296e+00  2.172e+00   1.978
## D.npnct06.log                            -2.086e+00  2.808e+00  -0.743
## D.npnct28.log                            -2.927e+00  1.100e+03  -0.003
## D.nuppr.log                              -4.728e+00  5.855e+00  -0.807
## D.npnct12.log                             5.770e-01  7.779e-01   0.742
## D.nchrs.log                               3.188e+00  6.726e+00   0.474
## D.nwrds.log                               5.364e-01  3.257e+00   0.165
## D.npnct09.log                            -8.613e+00  5.767e+02  -0.015
## D.ndgts.log                              -4.734e-01  5.623e-01  -0.842
## D.nwrds.unq.log                          -3.224e+00  3.807e+00  -0.847
## `carrier.fctrAT&T`                        3.238e-01  6.649e-01   0.487
## carrier.fctrOther                         1.542e+01  8.330e+02   0.019
## carrier.fctrSprint                        7.366e-01  9.023e-01   0.816
## `carrier.fctrT-Mobile`                   -1.554e+00  1.344e+00  -1.156
## carrier.fctrUnknown                      -2.862e-01  4.413e-01  -0.649
## carrier.fctrVerizon                       8.956e-01  7.079e-01   1.265
## cellular.fctr1                           -2.059e-01  6.100e-01  -0.338
## cellular.fctrUnknown                             NA         NA      NA
## D.npnct14.log                            -1.428e+00  8.443e-01  -1.691
## D.terms.n.post.stem                       7.631e+00  4.370e+00   1.746
## D.terms.n.post.stop                      -7.645e+00  4.327e+00  -1.767
## D.npnct05.log                            -1.914e+00  1.875e+00  -1.020
## `condition.fctrFor parts or not working` -3.578e-01  4.006e-01  -0.893
## `condition.fctrManufacturer refurbished` -4.505e-01  6.870e-01  -0.656
## condition.fctrNew                        -3.619e-01  3.005e-01  -1.204
## `condition.fctrNew other (see details)`  -5.057e-01  4.491e-01  -1.126
## `condition.fctrSeller refurbished`       -8.772e-01  4.604e-01  -1.905
## idseq.my                                 -6.635e-04  1.978e-04  -3.355
## startprice.log                           -9.830e-01  1.339e-01  -7.340
##                                          Pr(>|z|)    
## (Intercept)                              0.071878 .  
## biddable                                 2.35e-14 ***
## D.ratio.nstopwrds.nwrds                  0.018550 *  
## D.npnct15.log                            0.372048    
## D.T.excel                                0.333407    
## D.npnct03.log                            0.695736    
## D.T.screen                               0.813885    
## D.terms.n.stem.stop.Ratio                0.108341    
## D.T.use                                  0.045652 *  
## D.ratio.sum.TfIdf.nwrds                  0.304379    
## D.T.great                                0.254373    
## D.npnct01.log                            0.557619    
## D.T.good                                 0.190785    
## D.TfIdf.sum.stem.stop.Ratio              0.661550    
## D.T.scratch                              0.027870 *  
## D.P.air                                  0.484905    
## storage.fctr16                           0.647056    
## storage.fctr32                           0.959589    
## storage.fctr64                           0.125834    
## storage.fctrUnknown                      0.721788    
## D.P.mini                                 0.807303    
## D.T.work                                 0.798332    
## D.T.ipad                                 0.661570    
## D.npnct11.log                            0.890079    
## .rnorm                                   0.256045    
## D.npnct10.log                            0.990697    
## D.TfIdf.sum.post.stop                    0.704217    
## D.TfIdf.sum.post.stem                    0.803286    
## D.sum.TfIdf                                    NA    
## D.npnct13.log                            0.906601    
## D.T.condit                               0.689817    
## D.T.new                                  0.787941    
## D.npnct08.log                            0.333340    
## `prdline.my.fctriPad 1`                  0.374571    
## `prdline.my.fctriPad 2`                  0.197449    
## `prdline.my.fctriPad 3+`                 0.327754    
## prdline.my.fctriPadAir                   0.011825 *  
## prdline.my.fctriPadmini                  0.209222    
## `prdline.my.fctriPadmini 2+`             0.087941 .  
## color.fctrBlack                          0.538454    
## color.fctrGold                           0.532507    
## `color.fctrSpace Gray`                   0.596693    
## color.fctrWhite                          0.692839    
## D.npnct16.log                            0.685456    
## D.npnct24.log                            0.142847    
## D.nstopwrds.log                          0.047923 *  
## D.npnct06.log                            0.457566    
## D.npnct28.log                            0.997876    
## D.nuppr.log                              0.419396    
## D.npnct12.log                            0.458237    
## D.nchrs.log                              0.635512    
## D.nwrds.log                              0.869194    
## D.npnct09.log                            0.988084    
## D.ndgts.log                              0.399872    
## D.nwrds.unq.log                          0.397055    
## `carrier.fctrAT&T`                       0.626222    
## carrier.fctrOther                        0.985231    
## carrier.fctrSprint                       0.414290    
## `carrier.fctrT-Mobile`                   0.247753    
## carrier.fctrUnknown                      0.516603    
## carrier.fctrVerizon                      0.205786    
## cellular.fctr1                           0.735656    
## cellular.fctrUnknown                           NA    
## D.npnct14.log                            0.090852 .  
## D.terms.n.post.stem                      0.080725 .  
## D.terms.n.post.stop                      0.077267 .  
## D.npnct05.log                            0.307511    
## `condition.fctrFor parts or not working` 0.371786    
## `condition.fctrManufacturer refurbished` 0.511963    
## condition.fctrNew                        0.228507    
## `condition.fctrNew other (see details)`  0.260097    
## `condition.fctrSeller refurbished`       0.056764 .  
## idseq.my                                 0.000794 ***
## startprice.log                           2.13e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.4  on 975  degrees of freedom
## Residual deviance:  841.1  on 904  degrees of freedom
## AIC: 985.1
## 
## Number of Fisher Scoring iterations: 14
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-4.png) ![](ebayipads_txtterms_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6847826
## 3        0.2 0.7339286
## 4        0.3 0.7690783
## 5        0.4 0.7839305
## 6        0.5 0.7847222
## 7        0.6 0.7803681
## 8        0.7 0.7408377
## 9        0.8 0.6360947
## 10       0.9 0.4662162
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           451                            74
## 2         Y                           112                           339
##          Prediction
## Reference   N   Y
##         N 451  74
##         Y 112 339
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.094262e-01   6.143587e-01   7.833545e-01   8.336156e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   9.959402e-71   6.668282e-03 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-6.png) ![](ebayipads_txtterms_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6764200
## 3        0.2 0.7096135
## 4        0.3 0.7409836
## 5        0.4 0.7568209
## 6        0.5 0.7575000
## 7        0.6 0.7412399
## 8        0.7 0.7153076
## 9        0.8 0.6466877
## 10       0.9 0.4863884
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           388                            88
## 2         Y                           106                           303
##          Prediction
## Reference   N   Y
##         N 388  88
##         Y 106 303
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.807910e-01   5.576847e-01   7.520575e-01   8.076410e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   4.669336e-51   2.222645e-01 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.311                 0.175
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8761525                    0.5       0.7847222        0.7633192
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7833545             0.8336156     0.5226601   0.8391188
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5          0.7575         0.780791
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7520575              0.807641     0.5576847    985.1018
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0004195939     0.001563689
##                   label step_major step_minor     bgn    end elapsed
## 2      fit.models_1_glm          2          0  97.423 102.61   5.187
## 3 fit.models_1_bayesglm          3          0 102.611     NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log"
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

![](ebayipads_txtterms_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.5546  -0.7146  -0.2888   0.6735   2.5740  
## 
## Coefficients:
##                                           Estimate Std. Error z value
## (Intercept)                               0.362188   6.194353   0.058
## biddable                                  1.459201   0.187819   7.769
## D.ratio.nstopwrds.nwrds                  -0.912835   2.327208  -0.392
## D.npnct15.log                             1.182152   0.774323   1.527
## D.T.excel                                 0.325396   0.444681   0.732
## D.npnct03.log                             0.445299   1.573678   0.283
## D.T.screen                                0.100009   0.658937   0.152
## D.terms.n.stem.stop.Ratio                 1.322235   4.987976   0.265
## D.T.use                                   1.273897   0.622405   2.047
## D.ratio.sum.TfIdf.nwrds                   0.245754   0.393325   0.625
## D.T.great                                 0.564651   0.579705   0.974
## D.npnct01.log                             0.064311   0.522672   0.123
## D.T.good                                  0.806453   0.652268   1.236
## D.TfIdf.sum.stem.stop.Ratio               3.163707   3.422420   0.924
## D.T.scratch                               1.221956   0.740082   1.651
## D.P.air                                   0.417381   0.944358   0.442
## storage.fctr16                            0.180754   0.446232   0.405
## storage.fctr32                            0.036677   0.470277   0.078
## storage.fctr64                            0.767275   0.460265   1.667
## storage.fctrUnknown                       0.237572   0.608124   0.391
## D.P.mini                                  0.180390   0.775364   0.233
## D.T.work                                  0.025937   0.758149   0.034
## D.T.ipad                                 -0.264041   0.772145  -0.342
## D.npnct11.log                             0.139202   0.331633   0.420
## .rnorm                                   -0.104321   0.089190  -1.170
## D.npnct10.log                             0.870521   1.336571   0.651
## D.TfIdf.sum.post.stop                     0.101969   0.300044   0.340
## D.TfIdf.sum.post.stem                     0.108831   0.314361   0.346
## D.sum.TfIdf                               0.108831   0.314361   0.346
## D.npnct13.log                             0.093362   0.326867   0.286
## D.T.condit                               -0.287142   0.835637  -0.344
## D.T.new                                   0.051463   0.492242   0.105
## D.npnct08.log                            -0.626571   0.723805  -0.866
## `prdline.my.fctriPad 1`                   0.312114   0.420426   0.742
## `prdline.my.fctriPad 2`                   0.517839   0.421102   1.230
## `prdline.my.fctriPad 3+`                  0.397678   0.400117   0.994
## prdline.my.fctriPadAir                    1.057981   0.414317   2.554
## prdline.my.fctriPadmini                   0.498316   0.397081   1.255
## `prdline.my.fctriPadmini 2+`              0.737309   0.442571   1.666
## color.fctrBlack                          -0.143274   0.245663  -0.583
## color.fctrGold                           -0.235276   0.473132  -0.497
## `color.fctrSpace Gray`                    0.148538   0.307330   0.483
## color.fctrWhite                           0.093593   0.237942   0.393
## D.npnct16.log                             0.962781   1.911758   0.504
## D.npnct24.log                            -2.798740   2.971471  -0.942
## D.nstopwrds.log                           0.709490   0.655993   1.082
## D.npnct06.log                            -2.085277   2.144558  -0.972
## D.npnct28.log                            -0.066478   2.180954  -0.030
## D.nuppr.log                              -0.152886   0.520744  -0.294
## D.npnct12.log                             0.380989   0.709389   0.537
## D.nchrs.log                              -0.135032   0.501679  -0.269
## D.nwrds.log                               0.141627   0.767827   0.184
## D.npnct09.log                            -1.984417   5.160049  -0.385
## D.ndgts.log                               0.207059   0.398557   0.520
## D.nwrds.unq.log                          -0.360338   0.989602  -0.364
## `carrier.fctrAT&T`                        0.068465   0.761603   0.090
## carrier.fctrOther                         1.576461   1.715817   0.919
## carrier.fctrSprint                        0.428862   0.907203   0.473
## `carrier.fctrT-Mobile`                   -1.391579   1.134562  -1.227
## carrier.fctrUnknown                      -0.578856   0.777938  -0.744
## carrier.fctrVerizon                       0.497831   0.785960   0.633
## cellular.fctr1                            0.057840   0.738385   0.078
## cellular.fctrUnknown                      0.268591   0.846865   0.317
## D.npnct14.log                            -0.944405   0.746779  -1.265
## D.terms.n.post.stem                      -0.027060   0.189384  -0.143
## D.terms.n.post.stop                      -0.061837   0.188582  -0.328
## D.npnct05.log                            -2.118955   1.443672  -1.468
## `condition.fctrFor parts or not working` -0.325231   0.383239  -0.849
## `condition.fctrManufacturer refurbished` -0.301912   0.628863  -0.480
## condition.fctrNew                        -0.361606   0.293146  -1.234
## `condition.fctrNew other (see details)`  -0.446502   0.418412  -1.067
## `condition.fctrSeller refurbished`       -0.786494   0.433186  -1.816
## idseq.my                                 -0.000648   0.000192  -3.375
## startprice.log                           -0.959022   0.133142  -7.203
##                                          Pr(>|z|)    
## (Intercept)                              0.953374    
## biddable                                 7.90e-15 ***
## D.ratio.nstopwrds.nwrds                  0.694877    
## D.npnct15.log                            0.126838    
## D.T.excel                                0.464321    
## D.npnct03.log                            0.777202    
## D.T.screen                               0.879366    
## D.terms.n.stem.stop.Ratio                0.790944    
## D.T.use                                  0.040684 *  
## D.ratio.sum.TfIdf.nwrds                  0.532095    
## D.T.great                                0.330041    
## D.npnct01.log                            0.902073    
## D.T.good                                 0.216316    
## D.TfIdf.sum.stem.stop.Ratio              0.355275    
## D.T.scratch                              0.098716 .  
## D.P.air                                  0.658509    
## storage.fctr16                           0.685427    
## storage.fctr32                           0.937837    
## storage.fctr64                           0.095509 .  
## storage.fctrUnknown                      0.696045    
## D.P.mini                                 0.816032    
## D.T.work                                 0.972709    
## D.T.ipad                                 0.732383    
## D.npnct11.log                            0.674671    
## .rnorm                                   0.242138    
## D.npnct10.log                            0.514847    
## D.TfIdf.sum.post.stop                    0.733973    
## D.TfIdf.sum.post.stem                    0.729193    
## D.sum.TfIdf                              0.729193    
## D.npnct13.log                            0.775164    
## D.T.condit                               0.731132    
## D.T.new                                  0.916735    
## D.npnct08.log                            0.386675    
## `prdline.my.fctriPad 1`                  0.457860    
## `prdline.my.fctriPad 2`                  0.218801    
## `prdline.my.fctriPad 3+`                 0.320270    
## prdline.my.fctriPadAir                   0.010663 *  
## prdline.my.fctriPadmini                  0.209498    
## `prdline.my.fctriPadmini 2+`             0.095720 .  
## color.fctrBlack                          0.559749    
## color.fctrGold                           0.618996    
## `color.fctrSpace Gray`                   0.628869    
## color.fctrWhite                          0.694066    
## D.npnct16.log                            0.614535    
## D.npnct24.log                            0.346259    
## D.nstopwrds.log                          0.279452    
## D.npnct06.log                            0.330873    
## D.npnct28.log                            0.975683    
## D.nuppr.log                              0.769069    
## D.npnct12.log                            0.591222    
## D.nchrs.log                              0.787806    
## D.nwrds.log                              0.853659    
## D.npnct09.log                            0.700554    
## D.ndgts.log                              0.603397    
## D.nwrds.unq.log                          0.715765    
## `carrier.fctrAT&T`                       0.928370    
## carrier.fctrOther                        0.358210    
## carrier.fctrSprint                       0.636406    
## `carrier.fctrT-Mobile`                   0.219998    
## carrier.fctrUnknown                      0.456822    
## carrier.fctrVerizon                      0.526469    
## cellular.fctr1                           0.937563    
## cellular.fctrUnknown                     0.751122    
## D.npnct14.log                            0.206001    
## D.terms.n.post.stem                      0.886382    
## D.terms.n.post.stop                      0.742981    
## D.npnct05.log                            0.142171    
## `condition.fctrFor parts or not working` 0.396082    
## `condition.fctrManufacturer refurbished` 0.631162    
## condition.fctrNew                        0.217376    
## `condition.fctrNew other (see details)`  0.285911    
## `condition.fctrSeller refurbished`       0.069431 .  
## idseq.my                                 0.000739 ***
## startprice.log                           5.89e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  852.22  on 902  degrees of freedom
## AIC: 1000.2
## 
## Number of Fisher Scoring iterations: 17
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6784074
## 3        0.2 0.7223199
## 4        0.3 0.7655786
## 5        0.4 0.7787419
## 6        0.5 0.7819026
## 7        0.6 0.7722772
## 8        0.7 0.7332457
## 9        0.8 0.6156156
## 10       0.9 0.4398625
## 11       1.0 0.0000000
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                451
## 2         Y                                114
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 74
## 2                                337
##          Prediction
## Reference   N   Y
##         N 451  74
##         Y 114 337
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.073770e-01   6.100895e-01   7.812112e-01   8.316731e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   1.309621e-69   4.449971e-03 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6660988
## 3        0.2 0.7106299
## 4        0.3 0.7420417
## 5        0.4 0.7541766
## 6        0.5 0.7614213
## 7        0.6 0.7530696
## 8        0.7 0.7120116
## 9        0.8 0.6453674
## 10       0.9 0.4770642
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                397
## 2         Y                                109
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 79
## 2                                300
##          Prediction
## Reference   N   Y
##         N 397  79
##         Y 109 300
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.875706e-01   5.704749e-01   7.591217e-01   8.140900e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   5.004042e-54   3.442634e-02 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.156                 0.298
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8725837                    0.5       0.7819026        0.7684379
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7812112             0.8316731     0.5326115   0.8410552
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7614213        0.7875706
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7591217               0.81409     0.5704749    1000.216
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.007315812      0.01503357
##                   label step_major step_minor     bgn     end elapsed
## 3 fit.models_1_bayesglm          3          0 102.611 107.887   5.277
## 4   fit.models_1_glmnet          4          0 107.888      NA      NA
## [1] "fitting model: All.X.glmnet"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log"
```

```
## Loading required package: glmnet
## Loaded glmnet 2.0-2
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-12.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting alpha = 1, lambda = 0.0522 on full training set
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

![](ebayipads_txtterms_files/figure-html/fit.models_1-13.png) ![](ebayipads_txtterms_files/figure-html/fit.models_1-14.png) 

```
##             Length Class      Mode     
## a0           100   -none-     numeric  
## beta        7300   dgCMatrix  S4       
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
## xNames        73   -none-     character
## problemType    1   -none-     character
## tuneValue      2   data.frame list     
## obsLevels      2   -none-     character
## [1] "min lambda > lambdaOpt:"
##    (Intercept)       biddable       idseq.my startprice.log 
##   9.552277e-01   1.309794e+00  -9.792281e-05  -3.393767e-01 
## [1] "max lambda < lambdaOpt:"
##                            (Intercept) 
##                           1.001351e+01 
##                               biddable 
##                           1.473092e+00 
##                D.ratio.nstopwrds.nwrds 
##                          -1.291936e+01 
##                          D.npnct15.log 
##                           1.008154e+00 
##                              D.T.excel 
##                           4.698188e-01 
##                          D.npnct03.log 
##                           6.691643e-01 
##                             D.T.screen 
##                           2.502725e-01 
##              D.terms.n.stem.stop.Ratio 
##                           2.772960e+00 
##                                D.T.use 
##                           1.448813e+00 
##                D.ratio.sum.TfIdf.nwrds 
##                           6.969876e-01 
##                              D.T.great 
##                           7.350167e-01 
##                          D.npnct01.log 
##                          -1.646351e-01 
##                               D.T.good 
##                           1.040787e+00 
##            D.TfIdf.sum.stem.stop.Ratio 
##                           4.016821e+00 
##                            D.T.scratch 
##                           1.570324e+00 
##                                D.P.air 
##                           9.079118e-01 
##                         storage.fctr16 
##                           2.353969e-01 
##                         storage.fctr32 
##                           5.829740e-02 
##                         storage.fctr64 
##                           8.320905e-01 
##                    storage.fctrUnknown 
##                           2.673229e-01 
##                               D.P.mini 
##                           2.175160e-01 
##                               D.T.work 
##                           1.333821e-01 
##                               D.T.ipad 
##                          -2.889694e-01 
##                          D.npnct11.log 
##                           2.430051e-02 
##                                 .rnorm 
##                          -1.104180e-01 
##                          D.npnct10.log 
##                           5.709954e-01 
##                  D.TfIdf.sum.post.stop 
##                           4.283266e-01 
##                  D.TfIdf.sum.post.stem 
##                           1.957567e-05 
##                          D.npnct13.log 
##                           1.946778e-03 
##                             D.T.condit 
##                           3.568606e-01 
##                                D.T.new 
##                           1.332488e-01 
##                          D.npnct08.log 
##                          -6.523668e-01 
##                  prdline.my.fctriPad 1 
##                           4.062081e-01 
##                  prdline.my.fctriPad 2 
##                           6.217290e-01 
##                 prdline.my.fctriPad 3+ 
##                           4.776567e-01 
##                 prdline.my.fctriPadAir 
##                           1.178548e+00 
##                prdline.my.fctriPadmini 
##                           5.918282e-01 
##             prdline.my.fctriPadmini 2+ 
##                           8.616305e-01 
##                        color.fctrBlack 
##                          -1.591248e-01 
##                         color.fctrGold 
##                          -2.753249e-01 
##                   color.fctrSpace Gray 
##                           1.506299e-01 
##                        color.fctrWhite 
##                           9.541182e-02 
##                          D.npnct16.log 
##                           9.951767e-01 
##                          D.npnct24.log 
##                          -1.241272e+01 
##                        D.nstopwrds.log 
##                           3.520951e+00 
##                          D.npnct06.log 
##                          -2.225167e+00 
##                          D.npnct28.log 
##                          -6.618579e-01 
##                            D.nuppr.log 
##                          -1.068755e+00 
##                          D.npnct12.log 
##                           5.228279e-01 
##                            D.nwrds.log 
##                           3.870478e-01 
##                          D.npnct09.log 
##                          -3.213221e+00 
##                            D.ndgts.log 
##                          -1.262778e-01 
##                        D.nwrds.unq.log 
##                          -1.730880e+00 
##                       carrier.fctrAT&T 
##                           1.387528e-01 
##                      carrier.fctrOther 
##                           7.539458e+00 
##                     carrier.fctrSprint 
##                           5.467951e-01 
##                   carrier.fctrT-Mobile 
##                          -1.665228e+00 
##                    carrier.fctrUnknown 
##                          -5.168876e-01 
##                    carrier.fctrVerizon 
##                           6.030217e-01 
##                   cellular.fctrUnknown 
##                           2.600047e-01 
##                          D.npnct14.log 
##                          -1.075374e+00 
##                    D.terms.n.post.stem 
##                           5.568149e-02 
##                    D.terms.n.post.stop 
##                          -2.263768e-01 
##                          D.npnct05.log 
##                          -2.051269e+00 
## condition.fctrFor parts or not working 
##                          -3.901033e-01 
## condition.fctrManufacturer refurbished 
##                          -4.016063e-01 
##                      condition.fctrNew 
##                          -3.741282e-01 
##  condition.fctrNew other (see details) 
##                          -5.502636e-01 
##       condition.fctrSeller refurbished 
##                          -8.762562e-01 
##                               idseq.my 
##                          -6.532339e-04 
##                         startprice.log 
##                          -9.747200e-01 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6320953
## 3        0.2 0.6387509
## 4        0.3 0.7361111
## 5        0.4 0.7395143
## 6        0.5 0.7435610
## 7        0.6 0.7289003
## 8        0.7 0.5073650
## 9        0.8 0.3497268
## 10       0.9 0.2244094
## 11       1.0 0.0000000
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-16.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              415
## 2         Y                              119
##   sold.fctr.predict.All.X.glmnet.Y
## 1                              110
## 2                              332
##          Prediction
## Reference   N   Y
##         N 415 110
##         Y 119 332
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.653689e-01   5.273598e-01   7.374925e-01   7.916339e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   4.593282e-49   5.970449e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-17.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6321484
## 3        0.2 0.6365055
## 4        0.3 0.7367281
## 5        0.4 0.7675277
## 6        0.5 0.7721046
## 7        0.6 0.7548209
## 8        0.7 0.5489510
## 9        0.8 0.4015595
## 10       0.9 0.2370690
## 11       1.0 0.0000000
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              392
## 2         Y                               99
##   sold.fctr.predict.All.X.glmnet.Y
## 1                               84
## 2                              310
##          Prediction
## Reference   N   Y
##         N 392  84
##         Y  99 310
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.932203e-01   5.829804e-01   7.650169e-01   8.194558e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.393813e-56   3.007110e-01 
##       model_id model_method
## 1 All.X.glmnet       glmnet
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                      6.821                  1.94
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8401225                    0.5        0.743561         0.769473
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7374925             0.7916339     0.5352804   0.8482361
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7721046        0.7932203
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7650169             0.8194558     0.5829804
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.007872477      0.01795075
##                 label step_major step_minor     bgn     end elapsed
## 4 fit.models_1_glmnet          4          0 107.888 118.572  10.684
## 5  fit.models_1_rpart          5          0 118.573      NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0111 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-19.png) ![](ebayipads_txtterms_files/figure-html/fit.models_1-20.png) 

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
##       D.ratio.nstopwrds.nwrds < 0.269697  to the left,  improve=  8.111339, (0 missing)
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
##       startprice.log        < 4.923459  to the right, improve=38.162940, (0 missing)
##       idseq.my              < 920.5     to the right, improve=19.234430, (0 missing)
##       condition.fctrNew     < 0.5       to the right, improve= 3.803542, (0 missing)
##       cellular.fctrUnknown  < 0.5       to the right, improve= 3.795302, (0 missing)
##       D.TfIdf.sum.post.stop < 7.237556  to the left,  improve= 2.693422, (0 missing)
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
##       idseq.my               < 920.5     to the right, improve=17.345980, (0 missing)
##       prdline.my.fctriPadAir < 0.5       to the left,  improve= 5.716666, (0 missing)
##       startprice.log         < 6.024113  to the right, improve= 5.432188, (0 missing)
##       color.fctrSpace Gray   < 0.5       to the left,  improve= 4.308136, (0 missing)
##       color.fctrBlack        < 0.5       to the right, improve= 2.710165, (0 missing)
##   Surrogate splits:
##       storage.fctrUnknown                    < 0.5       to the right, agree=0.626, adj=0.103, (0 split)
##       startprice.log                         < 6.024113  to the right, agree=0.626, adj=0.103, (0 split)
##       condition.fctrFor parts or not working < 0.5       to the right, agree=0.620, adj=0.088, (0 split)
##       D.ratio.sum.TfIdf.nwrds                < 0.3691527 to the right, agree=0.613, adj=0.074, (0 split)
##       D.T.great                              < 0.4655933 to the right, agree=0.613, adj=0.074, (0 split)
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

![](ebayipads_txtterms_files/figure-html/fit.models_1-21.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_1-22.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_1-23.png) 

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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.429                 0.055
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8116461                    0.6       0.7679222        0.7981438
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7779982             0.8287575     0.5882503   0.8087593
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7742782        0.8056497
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7780145             0.8312324      0.605255
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01270241      0.02818363
##                label step_major step_minor     bgn    end elapsed
## 5 fit.models_1_rpart          5          0 118.573 123.45   4.877
## 6    fit.models_1_rf          6          0 123.450     NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log"
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

![](ebayipads_txtterms_files/figure-html/fit.models_1-24.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 37 on full training set
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-25.png) ![](ebayipads_txtterms_files/figure-html/fit.models_1-26.png) 

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
## importance        72   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                976   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            72   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-27.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.7989371
## 3        0.2 0.9415449
## 4        0.3 0.9815016
## 5        0.4 0.9988926
## 6        0.5 1.0000000
## 7        0.6 0.9988901
## 8        0.7 0.9437939
## 9        0.8 0.8435897
## 10       0.9 0.7402235
## 11       1.0 0.1050420
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-28.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_1-29.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6627810
## 3        0.2 0.7034884
## 4        0.3 0.7519466
## 5        0.4 0.7776428
## 6        0.5 0.7864583
## 7        0.6 0.7715458
## 8        0.7 0.7460545
## 9        0.8 0.7187970
## 10       0.9 0.5939597
## 11       1.0 0.0477327
```

![](ebayipads_txtterms_files/figure-html/fit.models_1-30.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   419
## 2         Y                                   107
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                    57
## 2                                   302
##          Prediction
## Reference   N   Y
##         N 419  57
##         Y 107 302
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.146893e-01   6.240072e-01   7.874927e-01   8.397715e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   5.523291e-67   1.301064e-04 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     12.717                 3.881
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.7838068
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9962275                     1      0.561723   0.8521579
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7864583        0.8146893
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7874927             0.8397715     0.6240072
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02492697      0.04716872
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    biddable, startprice.log
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               biddable, startprice.log
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         biddable, startprice.log
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           biddable, startprice.log
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 biddable, startprice.log, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr
## Low.cor.X.glm                                                                                                                                                                                                                                                          biddable, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
## All.X.glm                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
## All.X.bayesglm            biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
## All.X.glmnet              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
## All.X.no.rnorm.rpart              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
## All.X.no.rnorm.rf                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                       0                      0.323
## Random.myrandom_classfr                 0                      0.259
## Max.cor.Y.cv.0.rpart                    0                      0.624
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.464
## Max.cor.Y.rpart                         3                      1.013
## Max.cor.Y.glm                           1                      0.940
## Interact.High.cor.Y.glm                 1                      0.963
## Low.cor.X.glm                           1                      1.176
## All.X.glm                               1                      1.311
## All.X.bayesglm                          1                      2.156
## All.X.glmnet                            9                      6.821
## All.X.no.rnorm.rpart                    3                      1.429
## All.X.no.rnorm.rf                       3                     12.717
##                           min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                         0.003   0.5000000
## Random.myrandom_classfr                   0.002   0.5143786
## Max.cor.Y.cv.0.rpart                      0.011   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.008   0.8754662
## Max.cor.Y.rpart                           0.012   0.8085735
## Max.cor.Y.glm                             0.011   0.8349826
## Interact.High.cor.Y.glm                   0.015   0.8396790
## Low.cor.X.glm                             0.104   0.8658642
## All.X.glm                                 0.175   0.8761525
## All.X.bayesglm                            0.298   0.8725837
## All.X.glmnet                              1.940   0.8401225
## All.X.no.rnorm.rpart                      0.055   0.8116461
## All.X.no.rnorm.rf                         3.881   1.0000000
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6320953
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.4       0.8030303
## Max.cor.Y.rpart                              0.5       0.7517241
## Max.cor.Y.glm                                0.5       0.7471526
## Interact.High.cor.Y.glm                      0.5       0.7465116
## Low.cor.X.glm                                0.5       0.7808858
## All.X.glm                                    0.5       0.7847222
## All.X.bayesglm                               0.5       0.7819026
## All.X.glmnet                                 0.5       0.7435610
## All.X.no.rnorm.rpart                         0.6       0.7679222
## All.X.no.rnorm.rf                            0.5       1.0000000
##                           max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                0.5379098             0.5060449
## Random.myrandom_classfr          0.4620902             0.4304545
## Max.cor.Y.cv.0.rpart             0.5379098             0.5060449
## Max.cor.Y.cv.0.cp.0.rpart        0.8135246             0.7876443
## Max.cor.Y.rpart                  0.7745729             0.7513118
## Max.cor.Y.glm                    0.7694604             0.7449290
## Interact.High.cor.Y.glm          0.7704987             0.7491833
## Low.cor.X.glm                    0.7602360             0.7812112
## All.X.glm                        0.7633192             0.7833545
## All.X.bayesglm                   0.7684379             0.7812112
## All.X.glmnet                     0.7694730             0.7374925
## All.X.no.rnorm.rpart             0.7981438             0.7779982
## All.X.no.rnorm.rf                0.7838068             0.9962275
##                           max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                     0.5695455     0.0000000   0.5000000
## Random.myrandom_classfr               0.4939551     0.0000000   0.5214656
## Max.cor.Y.cv.0.rpart                  0.5695455     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8374971     0.6261780   0.8186805
## Max.cor.Y.rpart                       0.8043723     0.5393250   0.8157399
## Max.cor.Y.glm                         0.7984977     0.5343223   0.8432177
## Interact.High.cor.Y.glm               0.8024150     0.5354264   0.8384587
## Low.cor.X.glm                         0.8316731     0.5157767   0.8449025
## All.X.glm                             0.8336156     0.5226601   0.8391188
## All.X.bayesglm                        0.8316731     0.5326115   0.8410552
## All.X.glmnet                          0.7916339     0.5352804   0.8482361
## All.X.no.rnorm.rpart                  0.8287575     0.5882503   0.8087593
## All.X.no.rnorm.rf                     1.0000000     0.5617230   0.8521579
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6321484
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.6       0.7634961
## Max.cor.Y.rpart                              0.5       0.7705957
## Max.cor.Y.glm                                0.4       0.7701711
## Interact.High.cor.Y.glm                      0.4       0.7699877
## Low.cor.X.glm                                0.5       0.7623888
## All.X.glm                                    0.5       0.7575000
## All.X.bayesglm                               0.5       0.7614213
## All.X.glmnet                                 0.5       0.7721046
## All.X.no.rnorm.rpart                         0.6       0.7742782
## All.X.no.rnorm.rf                            0.5       0.7864583
##                           max.Accuracy.OOB max.AccuracyLower.OOB
## MFO.myMFO_classfr                0.5378531             0.5043611
## Random.myrandom_classfr          0.4621469             0.4289077
## Max.cor.Y.cv.0.rpart             0.5378531             0.5043611
## Max.cor.Y.cv.0.cp.0.rpart        0.7920904             0.7638373
## Max.cor.Y.rpart                  0.7954802             0.7673772
## Max.cor.Y.glm                    0.7875706             0.7591217
## Interact.High.cor.Y.glm          0.7887006             0.7603001
## Low.cor.X.glm                    0.7887006             0.7603001
## All.X.glm                        0.7807910             0.7520575
## All.X.bayesglm                   0.7875706             0.7591217
## All.X.glmnet                     0.7932203             0.7650169
## All.X.no.rnorm.rpart             0.8056497             0.7780145
## All.X.no.rnorm.rf                0.8146893             0.7874927
##                           max.AccuracyUpper.OOB max.Kappa.OOB
## MFO.myMFO_classfr                     0.5710923     0.0000000
## Random.myrandom_classfr               0.4956389     0.0000000
## Max.cor.Y.cv.0.rpart                  0.5710923     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8183833     0.5788853
## Max.cor.Y.rpart                       0.8215999     0.5865393
## Max.cor.Y.glm                         0.8140900     0.5726922
## Interact.High.cor.Y.glm               0.8151638     0.5745991
## Low.cor.X.glm                         0.8151638     0.5726857
## All.X.glm                             0.8076410     0.5576847
## All.X.bayesglm                        0.8140900     0.5704749
## All.X.glmnet                          0.8194558     0.5829804
## All.X.no.rnorm.rpart                  0.8312324     0.6052550
## All.X.no.rnorm.rf                     0.8397715     0.6240072
##                           max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                         NA              NA          NA
## Random.myrandom_classfr                   NA              NA          NA
## Max.cor.Y.cv.0.rpart                      NA              NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA              NA          NA
## Max.cor.Y.rpart                 0.0235259516     0.053611532          NA
## Max.cor.Y.glm                   0.0065013915     0.011856863    962.8176
## Interact.High.cor.Y.glm         0.0059986624     0.012913819    961.8295
## Low.cor.X.glm                   0.0136595379     0.027549590    980.6074
## All.X.glm                       0.0004195939     0.001563689    985.1018
## All.X.bayesglm                  0.0073158123     0.015033572   1000.2163
## All.X.glmnet                    0.0078724769     0.017950753          NA
## All.X.no.rnorm.rpart            0.0127024133     0.028183629          NA
## All.X.no.rnorm.rf               0.0249269655     0.047168722          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 6  fit.models_1_rf          6          0 123.45 139.69   16.24
## 7 fit.models_1_end          7          0 139.69     NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1  93.262 139.697  46.435
## 12 fit.models          7          2 139.697      NA      NA
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    biddable, startprice.log
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               biddable, startprice.log
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         biddable, startprice.log
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           biddable, startprice.log
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 biddable, startprice.log, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr
## Low.cor.X.glm                                                                                                                                                                                                                                                          biddable, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
## All.X.glm                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
## All.X.bayesglm            biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
## All.X.glmnet              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, .rnorm, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
## All.X.no.rnorm.rpart              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
## All.X.no.rnorm.rf                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
##                           max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                       0   0.5000000
## Random.myrandom_classfr                 0   0.5143786
## Max.cor.Y.cv.0.rpart                    0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart               0   0.8754662
## Max.cor.Y.rpart                         3   0.8085735
## Max.cor.Y.glm                           1   0.8349826
## Interact.High.cor.Y.glm                 1   0.8396790
## Low.cor.X.glm                           1   0.8658642
## All.X.glm                               1   0.8761525
## All.X.bayesglm                          1   0.8725837
## All.X.glmnet                            9   0.8401225
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
## Low.cor.X.glm                                0.5       0.7808858
## All.X.glm                                    0.5       0.7847222
## All.X.bayesglm                               0.5       0.7819026
## All.X.glmnet                                 0.5       0.7435610
## All.X.no.rnorm.rpart                         0.6       0.7679222
## All.X.no.rnorm.rf                            0.5       1.0000000
##                           max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                0.5379098     0.0000000   0.5000000
## Random.myrandom_classfr          0.4620902     0.0000000   0.5214656
## Max.cor.Y.cv.0.rpart             0.5379098     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart        0.8135246     0.6261780   0.8186805
## Max.cor.Y.rpart                  0.7745729     0.5393250   0.8157399
## Max.cor.Y.glm                    0.7694604     0.5343223   0.8432177
## Interact.High.cor.Y.glm          0.7704987     0.5354264   0.8384587
## Low.cor.X.glm                    0.7602360     0.5157767   0.8449025
## All.X.glm                        0.7633192     0.5226601   0.8391188
## All.X.bayesglm                   0.7684379     0.5326115   0.8410552
## All.X.glmnet                     0.7694730     0.5352804   0.8482361
## All.X.no.rnorm.rpart             0.7981438     0.5882503   0.8087593
## All.X.no.rnorm.rf                0.7838068     0.5617230   0.8521579
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6321484
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.6       0.7634961
## Max.cor.Y.rpart                              0.5       0.7705957
## Max.cor.Y.glm                                0.4       0.7701711
## Interact.High.cor.Y.glm                      0.4       0.7699877
## Low.cor.X.glm                                0.5       0.7623888
## All.X.glm                                    0.5       0.7575000
## All.X.bayesglm                               0.5       0.7614213
## All.X.glmnet                                 0.5       0.7721046
## All.X.no.rnorm.rpart                         0.6       0.7742782
## All.X.no.rnorm.rf                            0.5       0.7864583
##                           max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                0.5378531     0.0000000
## Random.myrandom_classfr          0.4621469     0.0000000
## Max.cor.Y.cv.0.rpart             0.5378531     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart        0.7920904     0.5788853
## Max.cor.Y.rpart                  0.7954802     0.5865393
## Max.cor.Y.glm                    0.7875706     0.5726922
## Interact.High.cor.Y.glm          0.7887006     0.5745991
## Low.cor.X.glm                    0.7887006     0.5726857
## All.X.glm                        0.7807910     0.5576847
## All.X.bayesglm                   0.7875706     0.5704749
## All.X.glmnet                     0.7932203     0.5829804
## All.X.no.rnorm.rpart             0.8056497     0.6052550
## All.X.no.rnorm.rf                0.8146893     0.6240072
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.myMFO_classfr                          3.0959752           333.3333333
## Random.myrandom_classfr                    3.8610039           500.0000000
## Max.cor.Y.cv.0.rpart                       1.6025641            90.9090909
## Max.cor.Y.cv.0.cp.0.rpart                  2.1551724           125.0000000
## Max.cor.Y.rpart                            0.9871668            83.3333333
## Max.cor.Y.glm                              1.0638298            90.9090909
## Interact.High.cor.Y.glm                    1.0384216            66.6666667
## Low.cor.X.glm                              0.8503401             9.6153846
## All.X.glm                                  0.7627765             5.7142857
## All.X.bayesglm                             0.4638219             3.3557047
## All.X.glmnet                               0.1466061             0.5154639
## All.X.no.rnorm.rpart                       0.6997901            18.1818182
## All.X.no.rnorm.rf                          0.0786349             0.2576656
##                            inv.aic.fit
## MFO.myMFO_classfr                   NA
## Random.myrandom_classfr             NA
## Max.cor.Y.cv.0.rpart                NA
## Max.cor.Y.cv.0.cp.0.rpart           NA
## Max.cor.Y.rpart                     NA
## Max.cor.Y.glm             0.0010386183
## Interact.High.cor.Y.glm   0.0010396853
## Low.cor.X.glm             0.0010197761
## All.X.glm                 0.0010151235
## All.X.bayesglm            0.0009997838
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

![](ebayipads_txtterms_files/figure-html/fit.models_2-1.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_2-2.png) 

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
## 13         All.X.no.rnorm.rf        0.8146893   0.8521579     0.6240072
## 12      All.X.no.rnorm.rpart        0.8056497   0.8087593     0.6052550
## 5            Max.cor.Y.rpart        0.7954802   0.8157399     0.5865393
## 11              All.X.glmnet        0.7932203   0.8482361     0.5829804
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7920904   0.8186805     0.5788853
## 8              Low.cor.X.glm        0.7887006   0.8449025     0.5726857
## 7    Interact.High.cor.Y.glm        0.7887006   0.8384587     0.5745991
## 6              Max.cor.Y.glm        0.7875706   0.8432177     0.5726922
## 10            All.X.bayesglm        0.7875706   0.8410552     0.5704749
## 9                  All.X.glm        0.7807910   0.8391188     0.5576847
## 1          MFO.myMFO_classfr        0.5378531   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.5378531   0.5000000     0.0000000
## 2    Random.myrandom_classfr        0.4621469   0.5214656     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 13          NA                    0.5
## 12          NA                    0.6
## 5           NA                    0.5
## 11          NA                    0.5
## 4           NA                    0.6
## 8     980.6074                    0.5
## 7     961.8295                    0.4
## 6     962.8176                    0.4
## 10   1000.2163                    0.5
## 9     985.1018                    0.5
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

![](ebayipads_txtterms_files/figure-html/fit.models_2-3.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.models_2-4.png) 

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
## importance        72   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                976   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            72   -none-     character
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
##                                          importance
## startprice.log                         100.00000000
## biddable                                75.77557376
## idseq.my                                61.89274395
## D.ratio.sum.TfIdf.nwrds                  6.55080925
## D.TfIdf.sum.stem.stop.Ratio              5.46221930
## color.fctrBlack                          5.42672497
## storage.fctr64                           4.54591071
## D.ratio.nstopwrds.nwrds                  4.48486137
## prdline.my.fctriPadAir                   4.21882380
## color.fctrWhite                          4.14701866
## prdline.my.fctriPad 1                    4.06373906
## D.sum.TfIdf                              3.99443151
## condition.fctrNew                        3.99149067
## D.TfIdf.sum.post.stem                    3.95656358
## storage.fctr16                           3.93700840
## color.fctrSpace Gray                     3.85738741
## D.TfIdf.sum.post.stop                    3.84367131
## cellular.fctr1                           3.74587984
## D.nuppr.log                              3.68920021
## D.nchrs.log                              3.42398279
## prdline.my.fctriPadmini                  3.32197629
## prdline.my.fctriPad 3+                   3.25104872
## carrier.fctrAT&T                         3.11118805
## cellular.fctrUnknown                     3.00719785
## carrier.fctrUnknown                      2.90117620
## storage.fctrUnknown                      2.86941294
## D.nstopwrds.log                          2.72664697
## carrier.fctrVerizon                      2.61940946
## D.nwrds.log                              2.60132737
## storage.fctr32                           2.55032142
## condition.fctrNew other (see details)    2.45350758
## prdline.my.fctriPad 2                    2.39999031
## D.T.use                                  2.25809141
## D.T.condit                               2.12215929
## condition.fctrSeller refurbished         2.10269385
## D.nwrds.unq.log                          2.04380750
## D.npnct11.log                            1.99781559
## D.T.scratch                              1.98124045
## prdline.my.fctriPadmini 2+               1.96047393
## D.terms.n.post.stem                      1.82074780
## D.terms.n.post.stop                      1.76498655
## D.T.good                                 1.68403568
## D.T.new                                  1.65458838
## D.T.screen                               1.61414375
## D.npnct13.log                            1.54450514
## D.T.ipad                                 1.51703617
## condition.fctrManufacturer refurbished   1.33277394
## condition.fctrFor parts or not working   1.31102748
## color.fctrGold                           1.27778210
## D.T.work                                 1.23378640
## carrier.fctrSprint                       1.01108476
## D.T.great                                0.95409170
## D.npnct15.log                            0.90427738
## D.ndgts.log                              0.81399761
## D.terms.n.stem.stop.Ratio                0.79621742
## D.T.excel                                0.78865866
## carrier.fctrT-Mobile                     0.66059489
## D.npnct14.log                            0.54157235
## D.npnct05.log                            0.50191492
## D.npnct01.log                            0.44250690
## D.npnct08.log                            0.43283592
## D.npnct24.log                            0.42904030
## D.npnct12.log                            0.40417016
## D.npnct10.log                            0.39734986
## D.npnct16.log                            0.27323339
## carrier.fctrOther                        0.16984240
## D.npnct03.log                            0.11963549
## D.P.mini                                 0.10241921
## D.npnct06.log                            0.09382797
## D.P.air                                  0.06502214
## D.npnct09.log                            0.00357743
## D.npnct28.log                            0.00000000
##                                        All.X.no.rnorm.rf.importance
## startprice.log                                         100.00000000
## biddable                                                75.77557376
## idseq.my                                                61.89274395
## D.ratio.sum.TfIdf.nwrds                                  6.55080925
## D.TfIdf.sum.stem.stop.Ratio                              5.46221930
## color.fctrBlack                                          5.42672497
## storage.fctr64                                           4.54591071
## D.ratio.nstopwrds.nwrds                                  4.48486137
## prdline.my.fctriPadAir                                   4.21882380
## color.fctrWhite                                          4.14701866
## prdline.my.fctriPad 1                                    4.06373906
## D.sum.TfIdf                                              3.99443151
## condition.fctrNew                                        3.99149067
## D.TfIdf.sum.post.stem                                    3.95656358
## storage.fctr16                                           3.93700840
## color.fctrSpace Gray                                     3.85738741
## D.TfIdf.sum.post.stop                                    3.84367131
## cellular.fctr1                                           3.74587984
## D.nuppr.log                                              3.68920021
## D.nchrs.log                                              3.42398279
## prdline.my.fctriPadmini                                  3.32197629
## prdline.my.fctriPad 3+                                   3.25104872
## carrier.fctrAT&T                                         3.11118805
## cellular.fctrUnknown                                     3.00719785
## carrier.fctrUnknown                                      2.90117620
## storage.fctrUnknown                                      2.86941294
## D.nstopwrds.log                                          2.72664697
## carrier.fctrVerizon                                      2.61940946
## D.nwrds.log                                              2.60132737
## storage.fctr32                                           2.55032142
## condition.fctrNew other (see details)                    2.45350758
## prdline.my.fctriPad 2                                    2.39999031
## D.T.use                                                  2.25809141
## D.T.condit                                               2.12215929
## condition.fctrSeller refurbished                         2.10269385
## D.nwrds.unq.log                                          2.04380750
## D.npnct11.log                                            1.99781559
## D.T.scratch                                              1.98124045
## prdline.my.fctriPadmini 2+                               1.96047393
## D.terms.n.post.stem                                      1.82074780
## D.terms.n.post.stop                                      1.76498655
## D.T.good                                                 1.68403568
## D.T.new                                                  1.65458838
## D.T.screen                                               1.61414375
## D.npnct13.log                                            1.54450514
## D.T.ipad                                                 1.51703617
## condition.fctrManufacturer refurbished                   1.33277394
## condition.fctrFor parts or not working                   1.31102748
## color.fctrGold                                           1.27778210
## D.T.work                                                 1.23378640
## carrier.fctrSprint                                       1.01108476
## D.T.great                                                0.95409170
## D.npnct15.log                                            0.90427738
## D.ndgts.log                                              0.81399761
## D.terms.n.stem.stop.Ratio                                0.79621742
## D.T.excel                                                0.78865866
## carrier.fctrT-Mobile                                     0.66059489
## D.npnct14.log                                            0.54157235
## D.npnct05.log                                            0.50191492
## D.npnct01.log                                            0.44250690
## D.npnct08.log                                            0.43283592
## D.npnct24.log                                            0.42904030
## D.npnct12.log                                            0.40417016
## D.npnct10.log                                            0.39734986
## D.npnct16.log                                            0.27323339
## carrier.fctrOther                                        0.16984240
## D.npnct03.log                                            0.11963549
## D.P.mini                                                 0.10241921
## D.npnct06.log                                            0.09382797
## D.P.air                                                  0.06502214
## D.npnct09.log                                            0.00357743
## D.npnct28.log                                            0.00000000
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
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 51
```

![](ebayipads_txtterms_files/figure-html/fit.models_2-5.png) ![](ebayipads_txtterms_files/figure-html/fit.models_2-6.png) ![](ebayipads_txtterms_files/figure-html/fit.models_2-7.png) ![](ebayipads_txtterms_files/figure-html/fit.models_2-8.png) ![](ebayipads_txtterms_files/figure-html/fit.models_2-9.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 66      10066         N                                    0.166
## 951     10951         N                                    0.036
## 285     10285         Y                                    0.984
## 1517    11517         N                                    0.512
## 1502    11502         N                                    0.516
## 1309    11309         N                                    0.536
## 851     10851         N                                    0.544
## 602     10602         N                                    0.552
## 512     10512         N                                    0.600
## 1836    11836         N                                    0.626
## 1526    11526         N                                    0.648
## 127     10127         N                                    0.654
## 1700    11700         N                                    0.836
## 1769    11769         N                                    0.878
## 199     10199         N                                    0.990
##      sold.fctr.predict.All.X.no.rnorm.rf
## 66                                     N
## 951                                    N
## 285                                    Y
## 1517                                   Y
## 1502                                   Y
## 1309                                   Y
## 851                                    Y
## 602                                    Y
## 512                                    Y
## 1836                                   Y
## 1526                                   Y
## 127                                    Y
## 1700                                   Y
## 1769                                   Y
## 199                                    Y
##      sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 66                                           TRUE
## 951                                          TRUE
## 285                                          TRUE
## 1517                                        FALSE
## 1502                                        FALSE
## 1309                                        FALSE
## 851                                         FALSE
## 602                                         FALSE
## 512                                         FALSE
## 1836                                        FALSE
## 1526                                        FALSE
## 127                                         FALSE
## 1700                                        FALSE
## 1769                                        FALSE
## 199                                         FALSE
##      sold.fctr.predict.All.X.no.rnorm.rf.error .label
## 66                                       0.000  10066
## 951                                      0.000  10951
## 285                                      0.000  10285
## 1517                                     0.012  11517
## 1502                                     0.016  11502
## 1309                                     0.036  11309
## 851                                      0.044  10851
## 602                                      0.052  10602
## 512                                      0.100  10512
## 1836                                     0.126  11836
## 1526                                     0.148  11526
## 127                                      0.154  10127
## 1700                                     0.336  11700
## 1769                                     0.378  11769
## 199                                      0.490  10199
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 1697    11697         Y                                    0.012
## 1359    11359         Y                                    0.024
## 962     10962         Y                                    0.028
## 1605    11605         Y                                    0.034
## 1212    11212         Y                                    0.040
## 846     10846         Y                                    0.040
##      sold.fctr.predict.All.X.no.rnorm.rf
## 1697                                   N
## 1359                                   N
## 962                                    N
## 1605                                   N
## 1212                                   N
## 846                                    N
##      sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 1697                                        FALSE
## 1359                                        FALSE
## 962                                         FALSE
## 1605                                        FALSE
## 1212                                        FALSE
## 846                                         FALSE
##      sold.fctr.predict.All.X.no.rnorm.rf.error
## 1697                                    -0.488
## 1359                                    -0.476
## 962                                     -0.472
## 1605                                    -0.466
## 1212                                    -0.460
## 846                                     -0.460
##     UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 693    10693         Y                                    0.260
## 369    10369         N                                    0.692
## 120    10120         N                                    0.776
## 529    10529         N                                    0.790
## 569    10569         N                                    0.842
## 488    10488         N                                    0.984
##     sold.fctr.predict.All.X.no.rnorm.rf
## 693                                   N
## 369                                   Y
## 120                                   Y
## 529                                   Y
## 569                                   Y
## 488                                   Y
##     sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 693                                        FALSE
## 369                                        FALSE
## 120                                        FALSE
## 529                                        FALSE
## 569                                        FALSE
## 488                                        FALSE
##     sold.fctr.predict.All.X.no.rnorm.rf.error
## 693                                    -0.240
## 369                                     0.192
## 120                                     0.276
## 529                                     0.290
## 569                                     0.342
## 488                                     0.484
##     UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 413    10413         N                                    0.964
## 526    10526         N                                    0.978
## 491    10491         N                                    0.978
## 283    10283         N                                    0.982
## 488    10488         N                                    0.984
## 199    10199         N                                    0.990
##     sold.fctr.predict.All.X.no.rnorm.rf
## 413                                   Y
## 526                                   Y
## 491                                   Y
## 283                                   Y
## 488                                   Y
## 199                                   Y
##     sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 413                                        FALSE
## 526                                        FALSE
## 491                                        FALSE
## 283                                        FALSE
## 488                                        FALSE
## 199                                        FALSE
##     sold.fctr.predict.All.X.no.rnorm.rf.error
## 413                                     0.464
## 526                                     0.478
## 491                                     0.478
## 283                                     0.482
## 488                                     0.484
## 199                                     0.490
```

![](ebayipads_txtterms_files/figure-html/fit.models_2-10.png) 

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
## 12 fit.models          7          2 139.697 155.048  15.351
## 13 fit.models          7          3 155.049      NA      NA
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

![](ebayipads_txtterms_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 155.049 159.191   4.142
## 14 fit.data.training          8          0 159.191      NA      NA
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
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_txtterms_files/figure-html/fit.data.training_0-1.png) 

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
## importance        72   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               1861   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            72   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_txtterms_files/figure-html/fit.data.training_0-2.png) 

```
##    threshold   f.score
## 1        0.0 0.6321205
## 2        0.1 0.8140085
## 3        0.2 0.9398907
## 4        0.3 0.9817352
## 5        0.4 0.9982589
## 6        0.5 1.0000000
## 7        0.6 0.9994183
## 8        0.7 0.9408867
## 9        0.8 0.8548602
## 10       0.9 0.7634795
## 11       1.0 0.1015453
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

![](ebayipads_txtterms_files/figure-html/fit.data.training_0-3.png) 

```
##   model_id model_method
## 1 Final.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.excel, D.npnct03.log, D.T.screen, D.terms.n.stem.stop.Ratio, D.T.use, D.ratio.sum.TfIdf.nwrds, D.T.great, D.npnct01.log, D.T.good, D.TfIdf.sum.stem.stop.Ratio, D.T.scratch, D.P.air, storage.fctr, D.P.mini, D.T.work, D.T.ipad, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.T.condit, D.T.new, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.npnct12.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     17.041                 9.403
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.8119301
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.9980198                     1     0.6177489
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.008238765      0.01670118
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn    end elapsed
## 14 fit.data.training          8          0 159.191 179.61  20.419
## 15 fit.data.training          8          1 179.611     NA      NA
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
##                                        All.X.no.rnorm.rf.importance
## startprice.log                                         100.00000000
## biddable                                                75.77557376
## idseq.my                                                61.89274395
## D.ratio.sum.TfIdf.nwrds                                  6.55080925
## D.TfIdf.sum.stem.stop.Ratio                              5.46221930
## prdline.my.fctriPadAir                                   4.21882380
## prdline.my.fctriPad 1                                    4.06373906
## D.ratio.nstopwrds.nwrds                                  4.48486137
## color.fctrWhite                                          4.14701866
## D.TfIdf.sum.post.stop                                    3.84367131
## cellular.fctr1                                           3.74587984
## color.fctrBlack                                          5.42672497
## prdline.my.fctriPad 3+                                   3.25104872
## storage.fctr64                                           4.54591071
## D.TfIdf.sum.post.stem                                    3.95656358
## D.nchrs.log                                              3.42398279
## carrier.fctrUnknown                                      2.90117620
## storage.fctr16                                           3.93700840
## D.sum.TfIdf                                              3.99443151
## condition.fctrNew                                        3.99149067
## prdline.my.fctriPadmini                                  3.32197629
## D.nstopwrds.log                                          2.72664697
## D.nuppr.log                                              3.68920021
## prdline.my.fctriPad 2                                    2.39999031
## color.fctrSpace Gray                                     3.85738741
## prdline.my.fctriPadmini 2+                               1.96047393
## D.nwrds.log                                              2.60132737
## storage.fctr32                                           2.55032142
## storage.fctrUnknown                                      2.86941294
## cellular.fctrUnknown                                     3.00719785
## carrier.fctrVerizon                                      2.61940946
## carrier.fctrAT&T                                         3.11118805
## D.T.use                                                  2.25809141
## condition.fctrNew other (see details)                    2.45350758
## D.T.condit                                               2.12215929
## condition.fctrSeller refurbished                         2.10269385
## D.terms.n.post.stem                                      1.82074780
## D.terms.n.post.stop                                      1.76498655
## D.nwrds.unq.log                                          2.04380750
## D.T.scratch                                              1.98124045
## D.npnct11.log                                            1.99781559
## condition.fctrFor parts or not working                   1.31102748
## D.T.good                                                 1.68403568
## condition.fctrManufacturer refurbished                   1.33277394
## D.T.excel                                                0.78865866
## D.npnct13.log                                            1.54450514
## D.T.great                                                0.95409170
## color.fctrGold                                           1.27778210
## D.T.new                                                  1.65458838
## D.T.ipad                                                 1.51703617
## D.T.screen                                               1.61414375
## D.T.work                                                 1.23378640
## D.ndgts.log                                              0.81399761
## D.npnct15.log                                            0.90427738
## carrier.fctrSprint                                       1.01108476
## carrier.fctrT-Mobile                                     0.66059489
## D.npnct16.log                                            0.27323339
## D.terms.n.stem.stop.Ratio                                0.79621742
## D.npnct05.log                                            0.50191492
## D.npnct08.log                                            0.43283592
## D.npnct14.log                                            0.54157235
## D.npnct24.log                                            0.42904030
## carrier.fctrOther                                        0.16984240
## D.npnct01.log                                            0.44250690
## D.npnct06.log                                            0.09382797
## D.npnct12.log                                            0.40417016
## D.P.mini                                                 0.10241921
## D.npnct10.log                                            0.39734986
## D.npnct03.log                                            0.11963549
## D.P.air                                                  0.06502214
## D.npnct28.log                                            0.00000000
## D.npnct09.log                                            0.00357743
##                                          importance Final.rf.importance
## startprice.log                         1.000000e+02        1.000000e+02
## biddable                               9.822028e+01        9.822028e+01
## idseq.my                               6.664811e+01        6.664811e+01
## D.ratio.sum.TfIdf.nwrds                5.501794e+00        5.501794e+00
## D.TfIdf.sum.stem.stop.Ratio            5.470859e+00        5.470859e+00
## prdline.my.fctriPadAir                 5.122997e+00        5.122997e+00
## prdline.my.fctriPad 1                  5.076772e+00        5.076772e+00
## D.ratio.nstopwrds.nwrds                4.620204e+00        4.620204e+00
## color.fctrWhite                        4.544942e+00        4.544942e+00
## D.TfIdf.sum.post.stop                  4.540872e+00        4.540872e+00
## cellular.fctr1                         4.301726e+00        4.301726e+00
## color.fctrBlack                        4.027819e+00        4.027819e+00
## prdline.my.fctriPad 3+                 3.895146e+00        3.895146e+00
## storage.fctr64                         3.854447e+00        3.854447e+00
## D.TfIdf.sum.post.stem                  3.853459e+00        3.853459e+00
## D.nchrs.log                            3.818113e+00        3.818113e+00
## carrier.fctrUnknown                    3.815033e+00        3.815033e+00
## storage.fctr16                         3.751024e+00        3.751024e+00
## D.sum.TfIdf                            3.668908e+00        3.668908e+00
## condition.fctrNew                      3.633870e+00        3.633870e+00
## prdline.my.fctriPadmini                3.506140e+00        3.506140e+00
## D.nstopwrds.log                        3.451131e+00        3.451131e+00
## D.nuppr.log                            3.439784e+00        3.439784e+00
## prdline.my.fctriPad 2                  3.421780e+00        3.421780e+00
## color.fctrSpace Gray                   3.280611e+00        3.280611e+00
## prdline.my.fctriPadmini 2+             2.880925e+00        2.880925e+00
## D.nwrds.log                            2.868460e+00        2.868460e+00
## storage.fctr32                         2.837569e+00        2.837569e+00
## storage.fctrUnknown                    2.610476e+00        2.610476e+00
## cellular.fctrUnknown                   2.601186e+00        2.601186e+00
## carrier.fctrVerizon                    2.322727e+00        2.322727e+00
## carrier.fctrAT&T                       2.242393e+00        2.242393e+00
## D.T.use                                2.239005e+00        2.239005e+00
## condition.fctrNew other (see details)  2.235349e+00        2.235349e+00
## D.T.condit                             2.226344e+00        2.226344e+00
## condition.fctrSeller refurbished       2.203554e+00        2.203554e+00
## D.terms.n.post.stem                    2.160570e+00        2.160570e+00
## D.terms.n.post.stop                    2.005976e+00        2.005976e+00
## D.nwrds.unq.log                        1.943754e+00        1.943754e+00
## D.T.scratch                            1.917683e+00        1.917683e+00
## D.npnct11.log                          1.833161e+00        1.833161e+00
## condition.fctrFor parts or not working 1.597595e+00        1.597595e+00
## D.T.good                               1.583586e+00        1.583586e+00
## condition.fctrManufacturer refurbished 1.496177e+00        1.496177e+00
## D.T.excel                              1.447960e+00        1.447960e+00
## D.npnct13.log                          1.402395e+00        1.402395e+00
## D.T.great                              1.393428e+00        1.393428e+00
## color.fctrGold                         1.376307e+00        1.376307e+00
## D.T.new                                1.367479e+00        1.367479e+00
## D.T.ipad                               1.350443e+00        1.350443e+00
## D.T.screen                             1.328451e+00        1.328451e+00
## D.T.work                               1.246854e+00        1.246854e+00
## D.ndgts.log                            1.116139e+00        1.116139e+00
## D.npnct15.log                          1.095564e+00        1.095564e+00
## carrier.fctrSprint                     1.046308e+00        1.046308e+00
## carrier.fctrT-Mobile                   8.766188e-01        8.766188e-01
## D.npnct16.log                          5.862696e-01        5.862696e-01
## D.terms.n.stem.stop.Ratio              5.595316e-01        5.595316e-01
## D.npnct05.log                          5.211130e-01        5.211130e-01
## D.npnct08.log                          4.850993e-01        4.850993e-01
## D.npnct14.log                          3.647107e-01        3.647107e-01
## D.npnct24.log                          3.386245e-01        3.386245e-01
## carrier.fctrOther                      3.349017e-01        3.349017e-01
## D.npnct01.log                          3.115916e-01        3.115916e-01
## D.npnct06.log                          3.042029e-01        3.042029e-01
## D.npnct12.log                          3.027261e-01        3.027261e-01
## D.P.mini                               2.438581e-01        2.438581e-01
## D.npnct10.log                          2.409854e-01        2.409854e-01
## D.npnct03.log                          1.677621e-01        1.677621e-01
## D.P.air                                4.503618e-02        4.503618e-02
## D.npnct28.log                          1.611318e-03        1.611318e-03
## D.npnct09.log                          0.000000e+00        0.000000e+00
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 51
```

![](ebayipads_txtterms_files/figure-html/fit.data.training_0-4.png) ![](ebayipads_txtterms_files/figure-html/fit.data.training_0-5.png) ![](ebayipads_txtterms_files/figure-html/fit.data.training_0-6.png) ![](ebayipads_txtterms_files/figure-html/fit.data.training_0-7.png) ![](ebayipads_txtterms_files/figure-html/fit.data.training_0-8.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1       10001         N                           0.228
## 2       10002         Y                           1.000
## 91      10091         Y                           1.000
## 1397    11397         N                           0.010
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

![](ebayipads_txtterms_files/figure-html/fit.data.training_0-9.png) 

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

![](ebayipads_txtterms_files/figure-html/fit.data.training_0-10.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 179.611 184.818   5.207
## 16  predict.data.new          9          0 184.819      NA      NA
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 51
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

![](ebayipads_txtterms_files/figure-html/predict.data.new-1.png) 

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

![](ebayipads_txtterms_files/figure-html/predict.data.new-2.png) 

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

![](ebayipads_txtterms_files/figure-html/predict.data.new-3.png) 

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

![](ebayipads_txtterms_files/figure-html/predict.data.new-4.png) 

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

![](ebayipads_txtterms_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1862    11862      <NA>                           0.280
## 1865    11865      <NA>                           0.500
## 1891    11891      <NA>                           0.858
## 2625    12625      <NA>                           0.380
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
## NA.12        NA      <NA>                              NA
## NA.319       NA      <NA>                              NA
## NA.341       NA      <NA>                              NA
## NA.352       NA      <NA>                              NA
## NA.615       NA      <NA>                              NA
## NA.618       NA      <NA>                              NA
##        sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## NA.12                        <NA>                                  NA
## NA.319                       <NA>                                  NA
## NA.341                       <NA>                                  NA
## NA.352                       <NA>                                  NA
## NA.615                       <NA>                                  NA
## NA.618                       <NA>                                  NA
##        sold.fctr.predict.Final.rf.error
## NA.12                                NA
## NA.319                               NA
## NA.341                               NA
## NA.352                               NA
## NA.615                               NA
## NA.618                               NA
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

![](ebayipads_txtterms_files/figure-html/predict.data.new-6.png) 

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
    print(sprintf("    All post-stop-words TfIDf terms for %s:", txt_var))
    myprint_df(glb_post_stop_words_terms_df_lst[[txt_var]])
    TfIdf_mtrx <- glb_post_stop_words_TfIdf_mtrx_lst[[txt_var]]
    print(glb_allobs_df[
        which(TfIdf_mtrx[, tail(glb_post_stop_words_terms_df_lst[[txt_var]], 1)$pos] > 0), 
                        c(glb_id_var, glb_txt_vars)])
    print(nrow(subset(glb_post_stop_words_terms_df_lst[[txt_var]], freq == 1)))
    #print(glb_allobs_df[which(TfIdf_mtrx[, 246] > 0), c(glb_id_var, glb_txt_vars)])
    #unlist(strsplit(glb_allobs_df[2157, "description"], ""))
    #glb_allobs_df[2442, c(glb_id_var, glb_txt_vars)]
    #TfIdf_mtrx[2442, TfIdf_mtrx[2442, ] > 0]  
    
    print(sprintf("    Top_n post-stop-words TfIDf terms for %s:", txt_var))
    tmp_df <- glb_post_stop_words_terms_df_lst[[txt_var]]
    top_n_vctr <- tmp_df$term[1:glb_top_n[[txt_var]]]
    tmp_freq1_df <- subset(tmp_df, freq == 1)
    tmp_freq1_df$top_n <- grepl(paste0(top_n_vctr, collapse="|"), tmp_freq1_df$term)
    print(subset(tmp_freq1_df, top_n == TRUE))
}
```

```
## [1] "    All post-stop-words TfIDf terms for descr.my:"
##              TfIdf      term freq pos
## condition 207.6392 condition  497 163
## new       125.1790       new  156 484
## used      122.6714      used  237 781
## good      120.6369      good  197 321
## scratches 113.4465 scratches  254 641
## screen    105.8086    screen  210 643
##              TfIdf     term freq pos
## air      14.780926      air   21  42
## little   12.739438   little   13 421
## properly  5.217255 properly    5 578
## auction   3.769610  auction    4  67
## card      2.485587     card    3 126
## consumer  1.137667 consumer    1 171
##              TfIdf     term freq pos
## said     1.1376668     said    1 634
## sales    1.1376668    sales    1 636
## seconds  1.1376668  seconds    1 651
## setup    1.1376668    setup    1 661
## shipment 1.1376668 shipment    1 668
## 79in     0.9480557     79in    1  16
##     UniqueID
## 520    10520
##                                                                                             descr.my
## 520 Apple iPad mini 1st Generation 16GB, Wi- Fi, 7.9in - Space Gray, great condition comes with the 
## [1] 268
## [1] "    Top_n post-stop-words TfIDf terms for descr.my:"
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
## [1] 976  66
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 13         All.X.no.rnorm.rf        0.8146893   0.8521579     0.6240072
## 12      All.X.no.rnorm.rpart        0.8056497   0.8087593     0.6052550
## 5            Max.cor.Y.rpart        0.7954802   0.8157399     0.5865393
## 11              All.X.glmnet        0.7932203   0.8482361     0.5829804
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7920904   0.8186805     0.5788853
## 8              Low.cor.X.glm        0.7887006   0.8449025     0.5726857
## 7    Interact.High.cor.Y.glm        0.7887006   0.8384587     0.5745991
## 6              Max.cor.Y.glm        0.7875706   0.8432177     0.5726922
## 10            All.X.bayesglm        0.7875706   0.8410552     0.5704749
## 9                  All.X.glm        0.7807910   0.8391188     0.5576847
## 1          MFO.myMFO_classfr        0.5378531   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.5378531   0.5000000     0.0000000
## 2    Random.myrandom_classfr        0.4621469   0.5214656     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 13          NA                    0.5
## 12          NA                    0.6
## 5           NA                    0.5
## 11          NA                    0.5
## 4           NA                    0.6
## 8     980.6074                    0.5
## 7     961.8295                    0.4
## 6     962.8176                    0.4
## 10   1000.2163                    0.5
## 9     985.1018                    0.5
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
## [1] "All.X.no.rnorm.rf OOB confusion matrix & accuracy: "
##          Prediction
## Reference   N   Y
##         N 419  57
##         Y 107 302
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
## 4                 30               106        0.7794118
## 5                 25               127        0.8355263
## 1                 21                76        0.7835052
## 6                 20               106        0.8412698
## 2                 18                81        0.8181818
## 7                 17                87        0.8365385
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
##                                        All.X.no.rnorm.rf.importance
## startprice.log                                         100.00000000
## biddable                                                75.77557376
## idseq.my                                                61.89274395
## D.ratio.sum.TfIdf.nwrds                                  6.55080925
## D.TfIdf.sum.stem.stop.Ratio                              5.46221930
## color.fctrBlack                                          5.42672497
## storage.fctr64                                           4.54591071
## D.ratio.nstopwrds.nwrds                                  4.48486137
## prdline.my.fctriPadAir                                   4.21882380
## color.fctrWhite                                          4.14701866
## prdline.my.fctriPad 1                                    4.06373906
## D.sum.TfIdf                                              3.99443151
## condition.fctrNew                                        3.99149067
## D.TfIdf.sum.post.stem                                    3.95656358
## storage.fctr16                                           3.93700840
## color.fctrSpace Gray                                     3.85738741
## D.TfIdf.sum.post.stop                                    3.84367131
## cellular.fctr1                                           3.74587984
## D.nuppr.log                                              3.68920021
## D.nchrs.log                                              3.42398279
## prdline.my.fctriPadmini                                  3.32197629
## prdline.my.fctriPad 3+                                   3.25104872
## carrier.fctrAT&T                                         3.11118805
## cellular.fctrUnknown                                     3.00719785
## carrier.fctrUnknown                                      2.90117620
## storage.fctrUnknown                                      2.86941294
## D.nstopwrds.log                                          2.72664697
## carrier.fctrVerizon                                      2.61940946
## D.nwrds.log                                              2.60132737
## storage.fctr32                                           2.55032142
## condition.fctrNew other (see details)                    2.45350758
## prdline.my.fctriPad 2                                    2.39999031
## D.T.use                                                  2.25809141
## D.T.condit                                               2.12215929
## condition.fctrSeller refurbished                         2.10269385
## D.nwrds.unq.log                                          2.04380750
## D.npnct11.log                                            1.99781559
## D.T.scratch                                              1.98124045
## prdline.my.fctriPadmini 2+                               1.96047393
## D.terms.n.post.stem                                      1.82074780
## D.terms.n.post.stop                                      1.76498655
## D.T.good                                                 1.68403568
## D.T.new                                                  1.65458838
## D.T.screen                                               1.61414375
## D.npnct13.log                                            1.54450514
## D.T.ipad                                                 1.51703617
## condition.fctrManufacturer refurbished                   1.33277394
## condition.fctrFor parts or not working                   1.31102748
## color.fctrGold                                           1.27778210
## D.T.work                                                 1.23378640
## carrier.fctrSprint                                       1.01108476
## D.T.great                                                0.95409170
## D.npnct15.log                                            0.90427738
## D.ndgts.log                                              0.81399761
## D.terms.n.stem.stop.Ratio                                0.79621742
## D.T.excel                                                0.78865866
## carrier.fctrT-Mobile                                     0.66059489
## D.npnct14.log                                            0.54157235
## D.npnct05.log                                            0.50191492
## D.npnct01.log                                            0.44250690
## D.npnct08.log                                            0.43283592
## D.npnct24.log                                            0.42904030
## D.npnct12.log                                            0.40417016
## D.npnct10.log                                            0.39734986
## D.npnct16.log                                            0.27323339
## carrier.fctrOther                                        0.16984240
## D.npnct03.log                                            0.11963549
## D.P.mini                                                 0.10241921
## D.npnct06.log                                            0.09382797
## D.P.air                                                  0.06502214
## D.npnct09.log                                            0.00357743
## D.npnct28.log                                            0.00000000
##                                          importance Final.rf.importance
## startprice.log                         1.000000e+02        1.000000e+02
## biddable                               9.822028e+01        9.822028e+01
## idseq.my                               6.664811e+01        6.664811e+01
## D.ratio.sum.TfIdf.nwrds                5.501794e+00        5.501794e+00
## D.TfIdf.sum.stem.stop.Ratio            5.470859e+00        5.470859e+00
## color.fctrBlack                        4.027819e+00        4.027819e+00
## storage.fctr64                         3.854447e+00        3.854447e+00
## D.ratio.nstopwrds.nwrds                4.620204e+00        4.620204e+00
## prdline.my.fctriPadAir                 5.122997e+00        5.122997e+00
## color.fctrWhite                        4.544942e+00        4.544942e+00
## prdline.my.fctriPad 1                  5.076772e+00        5.076772e+00
## D.sum.TfIdf                            3.668908e+00        3.668908e+00
## condition.fctrNew                      3.633870e+00        3.633870e+00
## D.TfIdf.sum.post.stem                  3.853459e+00        3.853459e+00
## storage.fctr16                         3.751024e+00        3.751024e+00
## color.fctrSpace Gray                   3.280611e+00        3.280611e+00
## D.TfIdf.sum.post.stop                  4.540872e+00        4.540872e+00
## cellular.fctr1                         4.301726e+00        4.301726e+00
## D.nuppr.log                            3.439784e+00        3.439784e+00
## D.nchrs.log                            3.818113e+00        3.818113e+00
## prdline.my.fctriPadmini                3.506140e+00        3.506140e+00
## prdline.my.fctriPad 3+                 3.895146e+00        3.895146e+00
## carrier.fctrAT&T                       2.242393e+00        2.242393e+00
## cellular.fctrUnknown                   2.601186e+00        2.601186e+00
## carrier.fctrUnknown                    3.815033e+00        3.815033e+00
## storage.fctrUnknown                    2.610476e+00        2.610476e+00
## D.nstopwrds.log                        3.451131e+00        3.451131e+00
## carrier.fctrVerizon                    2.322727e+00        2.322727e+00
## D.nwrds.log                            2.868460e+00        2.868460e+00
## storage.fctr32                         2.837569e+00        2.837569e+00
## condition.fctrNew other (see details)  2.235349e+00        2.235349e+00
## prdline.my.fctriPad 2                  3.421780e+00        3.421780e+00
## D.T.use                                2.239005e+00        2.239005e+00
## D.T.condit                             2.226344e+00        2.226344e+00
## condition.fctrSeller refurbished       2.203554e+00        2.203554e+00
## D.nwrds.unq.log                        1.943754e+00        1.943754e+00
## D.npnct11.log                          1.833161e+00        1.833161e+00
## D.T.scratch                            1.917683e+00        1.917683e+00
## prdline.my.fctriPadmini 2+             2.880925e+00        2.880925e+00
## D.terms.n.post.stem                    2.160570e+00        2.160570e+00
## D.terms.n.post.stop                    2.005976e+00        2.005976e+00
## D.T.good                               1.583586e+00        1.583586e+00
## D.T.new                                1.367479e+00        1.367479e+00
## D.T.screen                             1.328451e+00        1.328451e+00
## D.npnct13.log                          1.402395e+00        1.402395e+00
## D.T.ipad                               1.350443e+00        1.350443e+00
## condition.fctrManufacturer refurbished 1.496177e+00        1.496177e+00
## condition.fctrFor parts or not working 1.597595e+00        1.597595e+00
## color.fctrGold                         1.376307e+00        1.376307e+00
## D.T.work                               1.246854e+00        1.246854e+00
## carrier.fctrSprint                     1.046308e+00        1.046308e+00
## D.T.great                              1.393428e+00        1.393428e+00
## D.npnct15.log                          1.095564e+00        1.095564e+00
## D.ndgts.log                            1.116139e+00        1.116139e+00
## D.terms.n.stem.stop.Ratio              5.595316e-01        5.595316e-01
## D.T.excel                              1.447960e+00        1.447960e+00
## carrier.fctrT-Mobile                   8.766188e-01        8.766188e-01
## D.npnct14.log                          3.647107e-01        3.647107e-01
## D.npnct05.log                          5.211130e-01        5.211130e-01
## D.npnct01.log                          3.115916e-01        3.115916e-01
## D.npnct08.log                          4.850993e-01        4.850993e-01
## D.npnct24.log                          3.386245e-01        3.386245e-01
## D.npnct12.log                          3.027261e-01        3.027261e-01
## D.npnct10.log                          2.409854e-01        2.409854e-01
## D.npnct16.log                          5.862696e-01        5.862696e-01
## carrier.fctrOther                      3.349017e-01        3.349017e-01
## D.npnct03.log                          1.677621e-01        1.677621e-01
## D.P.mini                               2.438581e-01        2.438581e-01
## D.npnct06.log                          3.042029e-01        3.042029e-01
## D.P.air                                4.503618e-02        4.503618e-02
## D.npnct09.log                          0.000000e+00        0.000000e+00
## D.npnct28.log                          1.611318e-03        1.611318e-03
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

![](ebayipads_txtterms_files/figure-html/predict.data.new-7.png) 

```r
if (glb_is_classification)
    print(table(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)]))
```

```
## 
##   N   Y 
## 530 268
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
## 16     predict.data.new          9          0 184.819 190.592   5.773
## 17 display.session.info         10          0 190.593      NA      NA
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
## 5         extract.features          3          0  13.792  64.848  51.056
## 11              fit.models          7          1  93.262 139.697  46.435
## 10              fit.models          7          0  68.280  93.261  24.981
## 14       fit.data.training          8          0 159.191 179.610  20.419
## 12              fit.models          7          2 139.697 155.048  15.351
## 16        predict.data.new          9          0 184.819 190.592   5.773
## 15       fit.data.training          8          1 179.611 184.818   5.207
## 13              fit.models          7          3 155.049 159.191   4.142
## 2             inspect.data          2          0   8.702  12.646   3.944
## 8          select.features          5          0  65.935  67.440   1.505
## 6             cluster.data          4          0  64.848  65.794   0.946
## 9  partition.data.training          6          0  67.441  68.280   0.839
## 3               scrub.data          2          1  12.647  13.347   0.700
## 4           transform.data          2          2  13.348  13.792   0.444
## 1              import.data          1          0   8.320   8.701   0.381
## 7      manage.missing.data          4          1  65.794  65.934   0.141
##    duration
## 5    51.056
## 11   46.435
## 10   24.981
## 14   20.419
## 12   15.351
## 16    5.773
## 15    5.207
## 13    4.142
## 2     3.944
## 8     1.505
## 6     0.946
## 9     0.839
## 3     0.700
## 4     0.444
## 1     0.381
## 7     0.140
## [1] "Total Elapsed Time: 190.592 secs"
```

![](ebayipads_txtterms_files/figure-html/display.session.info-1.png) 

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
##  [7] MASS_7.3-42         rpart.plot_1.5.2    rpart_4.1-10       
## [10] ROCR_1.0-7          gplots_2.17.0       sampling_2.7       
## [13] tm_0.6-2            NLP_0.1-8           stringr_1.0.0      
## [16] dplyr_0.4.2         plyr_1.8.3          sqldf_0.4-10       
## [19] RSQLite_1.0.0       DBI_0.3.1           gsubfn_0.6-6       
## [22] proto_0.3-10        reshape2_1.4.1      doMC_1.3.3         
## [25] iterators_1.0.7     foreach_1.4.2       doBy_4.5-13        
## [28] survival_2.38-3     caret_6.0-47        ggplot2_1.0.1      
## [31] lattice_0.20-33    
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
