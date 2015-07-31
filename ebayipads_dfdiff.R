setwd("~/Documents/Work/Courses/MIT/Analytics_Edge_15_071x/Assignments/Kaggle_eBay_iPads")

A_df_fnm <- "0_83458_template_Final_glm_submit.csv"
B_df_fnm <- "0_70111_splogdiff_Final_gbm_submit.csv"
mrg_by_var <- "UniqueID"
plot_var <- "Probability1"

A_df <- read.csv(file=A_df_fnm)
B_df <- read.csv(file=B_df_fnm)

AB_df <- merge(A_df, B_df, by=mrg_by_var, all=TRUE, suffixes=c(".A", ".B"))

print(ggplot(data=AB_df, mapping = aes(x=)))

# require(GGally)
# ggparcoord()
