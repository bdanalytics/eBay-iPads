split.sample:
prdline.my          .n.Tst .n.OOB .freqRatio.Tst .freqRatio.OOB
2            iPad 2    154    138     0.19298246    0.157175399
7         iPad mini    114    144     0.14285714    0.164009112
1            iPad 1     89    105     0.11152882    0.119589977
11          Unknown     87     93     0.10902256    0.105922551
5          iPad Air     75     77     0.09398496    0.087699317
4            iPad 4     68     78     0.08521303    0.088838269
6        iPad Air 2     62     79     0.07769424    0.089977221
8       iPad mini 2     56     54     0.07017544    0.061503417
3            iPad 3     55     72     0.06892231    0.082004556
9       iPad mini 3     38     34     0.04761905    0.038724374
10 iPad mini Retina     NA      4             NA    0.004555809

strata:
prdline.my          .n.Tst .n.OOB .freqRatio.Tst .freqRatio.OOB
2            iPad 2    154    122     0.19298246    0.136771300
8         iPad mini    114    132     0.14285714    0.147982063
1            iPad 1     89    113     0.11152882    0.126681614
12          Unknown     87     94     0.10902256    0.105381166
6          iPad Air     75     92     0.09398496    0.103139013
4            iPad 4     68     81     0.08521303    0.090807175
7        iPad Air 2     62     82     0.07769424    0.091928251
9       iPad mini 2     56     57     0.07017544    0.063901345
3            iPad 3     55     68     0.06892231    0.076233184
10      iPad mini 3     38     46     0.04761905    0.051569507
11 iPad mini Retina     NA      4             NA    0.004484305
5            iPad 5     NA      1             NA    0.001121076

as.numeric(performance(prediction(
    glb_OOBobs_df[, "sold.fctr.predict.All.Interact.X.gbm.prob"],
    glb_OOBobs_df[, glb_rsp_var]), "auc")@y.values)

print(myplot_prediction_classification(
    df=subset(glb_OOBobs_df, (prdl.my.descr.fctr == "iPadAir#0") & (biddable == 0)),
    feat_x="startprice.diff",
    feat_y="idseq.my",
    rsp_var=glb_rsp_var,
    rsp_var_out=paste0(glb_rsp_var_out, glb_sel_mdl_id),
    id_vars=glb_id_var,
    prob_threshold=0.5)
      #               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
)
require(GGally)
ggparcoord(data=subset(glb_OOBobs_df, (prdl.my.descr.fctr == "iPadAir#0") & (biddable == 0)),
           columns = 1:4,
           groupColumn = paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate"))
grep(paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate"), names(glb_allobs_df), fixed=TRUE)
df=subset(glb_OOBobs_df, (prdl.my.descr.fctr == "iPadAir#0") & (biddable == 0))
ggparcoord(data=df,
           columns = 28:32,
           groupColumn = 175)