#~ ~ ~ Load in required packages ~ ~ ~
library(haven)
library(dplyr)
library(tableone)
library(tidyr)
library(survival)
library(survminer)
library(forcats)
library(tidycmprsk)
library(ggsurvfit)
library(MASS)
library(car)
library(modelsummary)

#~ ~ ~ Load in raw data ~ ~ ~
df_brey<-read_sas(data_file = "C:/Users/btomczyk/Desktop/Capstone Project/Breyanzi/bms_brey_sep2026.sas7bdat",
                   catalog_file = "C:/Users/btomczyk/Desktop/Capstone Project/Breyanzi/formats.sas7bcat")

df_kym<-read_sas(data_file = "C:/Users/btomczyk/Desktop/Capstone Project/Kymriah/nvs_kym_aug2026.sas7bdat",
                   catalog_file = "C:/Users/btomczyk/Desktop/Capstone Project/Kymriah/formats.sas7bcat")

df_yes<-read_sas(data_file = "C:/Users/btomczyk/Desktop/Capstone Project/Yescarta/kite_july2026_deliverable.sas7bdat",
                      catalog_file = "C:/Users/btomczyk/Desktop/Capstone Project/Yescarta/formats.sas7bcat")

#~ ~ ~ Create dataset ~ ~ ~

#Filter Breyanzi dataset down to variables needed
selected_brey_cols<-c("did_bms_brey","age", "cntembargo", "center_country_tx", "pfs", "pfsstat",
                      "intpfs", "dead", "deadstat", "inctsurv", "sex", "ct_ecogpr", "intdxct",
                      "ct_lymstgdx","rel","relstat", "ct_exnodpr", "ct_enbraindx","ct_encsfdx","ct_enlmgdx", "ct_elevatedldhpr", "ct_subdis", "ct_ntreatpr", "ct_priortxtype", "ct_treatbg", "ct_lympetbresp_1y","intctpetbrsp_l"
, "ct_crsgrade_h1","ct_crsgrade_100d","ct_crsgrade_6m","ct_crsgrade_1y","ct_neutoxgrade_100d","ct_neutoxgrade_6m","ct_neutoxgrade_1y")
df_brey_filter<-dplyr::select(df_brey, all_of(selected_brey_cols))

#Filter Kymriah dataset down to variables needed
selected_kym_cols<-c("did_nov_kym","age", "Cntembargo", "center_country_tx", "pfs", "pfsstat",
                      "intpfs", "dead", "deadstat", "inctsurv", "sex", "ct_ecogpr", "intdxct",
                      "ct_lymstgdx","rel","relstat", "ct_exnodpr", "ct_enbraindx","ct_encsfdx","ct_enlmgdx","ct_elevatedldhpr", "ct_subdis", "ct_ntreatpr","ct_priortxtype", "ct_treatbg", "ct_lympetbresp_1y","intctpetbrsp_l", "ct_crsgrade_h1","ct_crsgrade_100d","ct_crsgrade_6m","ct_crsgrade_1y", "ct_neutoxgrade_100d","ct_neutoxgrade_6m","ct_neutoxgrade_1y")
df_kym_filter<-dplyr::select(df_kym, selected_kym_cols)

#Filter Yescarta dataset down to variables needed
selected_yes_cols<-c("did_kite_yes","age", "cntEmbargo", "center_country_tx", "pfs", "pfsstat",
                      "intpfs", "dead", "deadstat", "inctsurv", "sex", "ct_ecogpr", "intdxct",
                      "ct_lymstgdx","rel","relstat", "ct_exnodpr", "ct_enbraindx","ct_encsfdx","ct_enlmgdx", "ct_elevatedldhpr","ct_ldhpr", "ct_subdis", "ct_ntreatpr", "ct_priortxtype", "ct_treatbg", "ct_lympetbresp_1y","intctpetbrsp_l", "ct_crsgrade_h1","ct_crsgrade_100d","ct_crsgrade_6m","ct_crsgrade_1y","ct_neutoxgrade_100d","ct_neutoxgrade_6m","ct_neutoxgrade_1y")
df_yes_filter<-dplyr::select(df_yes, selected_yes_cols)

# ~ ~ ~ Rename variables to have consistent names ~ ~ ~

#cntembargo
df_kym_filter<-rename(df_kym_filter, cntembargo = Cntembargo)
df_yes_filter<-rename(df_yes_filter, cntembargo = cntEmbargo)

#did
df_brey_filter<-rename(df_brey_filter, did = did_bms_brey)
df_kym_filter<-rename(df_kym_filter, did = did_nov_kym)
df_yes_filter<-rename(df_yes_filter, did = did_kite_yes)

#~ ~ ~ Data Cleaning/Transformation ~ ~ ~
#Create product indicator
df_brey_filter$product<-"Breyanzi"
df_kym_filter$product<-"Kymriah"
df_yes_filter$product<-"Yescarta"

#Bind datasets into one dataset
df_cart<-bind_rows(df_brey_filter, df_kym_filter, df_yes_filter)

#Convert labels to R format
df_cart<-as.data.frame(df_cart)
df_cart<-as_factor(df_cart, levels="labels")

#Transform embargo status variable
df_cart$cntembargo[is.na(df_cart$cntembargo)]<-"not embargoed"

#Create indicator for ECOG
df_cart$ecog_s<-as.integer(df_cart$ct_ecogpr == "1 1, Symptomatic but completely ambulatory" | df_cart$ct_ecogpr == "2 2, Symptomatic, < 50% in bed during the day" | df_cart$ct_ecogpr == "3 3, Symptomatic, > 50% in bed, but not bedbound"  | df_cart$ct_ecogpr == "4 4, Bedbound")

#Create indicator for prior HCT
df_cart$hct_any<-as.integer(df_cart$ct_priortxtype != "0 No prior HCT")

#Create indicator for extranodal involvement CNS
df_cart<-df_cart %>%
  mutate(
    ei_cns = case_when(
      ct_enbraindx == "1 Yes" | ct_encsfdx == "1 Yes" | ct_enlmgdx == "1 Yes" ~ 1,
      ct_enbraindx == "0 No" & ct_encsfdx == "0 No" & ct_enlmgdx == "0 No" ~ 0,
      TRUE ~ NA_real_
    )
  )

#Create indicator for prior lines of therapy
df_cart$numlinestherapy<-ifelse(df_cart$ct_ntreatpr <= 2, 0, ifelse(df_cart$ct_ntreatpr <= 4, 1, 2))
df_cart$numlinestherapy<-factor(
  df_cart$numlinestherapy,
  levels = c(0,1,2),
  labels = c("1-2 Prior Lines", "3-4 Prior Lines", ">=5 Prior Lines")
)

#Recode Continued Complete Remission into Complete Remission
df_cart<-df_cart %>%
  mutate(
    ct_lympetbresp_1y = fct_collapse(ct_lympetbresp_1y,
    "2 Complete remission (CR)" = c(
      "1 Continued complete remission (CCR)",
      "2 Complete remission (CR)"
    )
  )
)

#Neurotoxicity coding
tox_vars<-sapply(df_cart[c("ct_neutoxgrade_100d","ct_neutoxgrade_6m","ct_neutoxgrade_1y")], as.character)
tox_vars[tox_vars %in% c("6 Under review", "7 Not Reported")]<-NA
tox_vars<-matrix(as.numeric(sub("^([0-9]+).*", "\\1", tox_vars)), ncol=3)
tox_max<-pmax(tox_vars[,1], tox_vars[,2], tox_vars[,3], na.rm=TRUE)
tox_any<-ifelse(is.na(tox_max), NA, ifelse(tox_max == 0, 0, 1))
df_cart$ntx_any<-factor(
  tox_any,
  levels = c(0,1),
  labels = c("0 No neurotoxicity", "1-5 Had Neurotoxicity")
)

#CRS grade coding
crs_vars<-sapply(df_cart[c("ct_crsgrade_100d","ct_crsgrade_6m","ct_crsgrade_1y")], as.character)
crs_vars[crs_vars %in% c("6 Under review", "7 Not Reported")]<-NA
crs_vars<-matrix(as.numeric(sub("^([0-9]+).*", "\\1", crs_vars)), ncol=3)
crs_max<-pmax(crs_vars[,1], crs_vars[,2], crs_vars[,3], na.rm=TRUE)
crs_any<-ifelse(is.na(crs_max), NA, ifelse(crs_max == 0, 0, 1))
df_cart$crs_fever_any<-factor(
  crs_any,
  levels = c(0,1),
  labels = c("0 No CRS", "1-5 CRS Present")
)

#Rescale intdxct into years
df_cart$intdxct<-df_cart$intdxct/365

#Check missingness
colSums(is.na(df_cart))

#~ ~ ~ Create Table 1 before further Cohort filtering ~ ~ ~
varsToFactor<-c("center_country_tx","sex","product", "ecog_s","ct_lymstgdx", "ct_exnodpr","ei_cns", "numlinestherapy","ntx_any", "ct_elevatedldhpr", "ct_subdis", "hct_any", "ct_treatbg", "ct_lympetbresp_1y", "crs_fever_any")
vars<-c("age", "center_country_tx", "sex","product", "ecog_s", "intdxct",
        "ct_lymstgdx", "ct_exnodpr","ei_cns", "ct_elevatedldhpr", "ct_subdis", "numlinestherapy", "hct_any", "ct_treatbg", "ct_lympetbresp_1y","intctpetbrsp_l","crs_fever_any","ntx_any")
tableone<-CreateTableOne(vars = vars, factorVars = varsToFactor, data=df_cart,
               test = FALSE, includeNA = TRUE, addOverall = TRUE)
write.csv(print(tableone, varLabels=TRUE, useLabels=TRUE, showAllLevels=TRUE), file="table1.csv")

#~ ~ ~ Further Cohort Filtering ~ ~ ~

#Create row tracker for exclusion set
df_cart <- df_cart %>% mutate(row_id = row_number())

#Remove those with wrong subtype
df_cart_f<-df_cart %>%
  filter(
    as.numeric(sub("^([0-9]+).*", "\\1", ct_subdis)) %in% c(107, 120, 163, 1820, 1821, 1822, 1823, 1830, 1831, 1837, 1838)
  )


#Group subdiseases
df_cart_f<-df_cart_f %>%
  mutate(
    ct_subdis = fct_collapse(ct_subdis,
                          "DLBCL and Related Subtypes" = c(
                            "107 Diffuse, large B-cell lymphoma, NOS",
                            "120 T-cell/histiocytic rich large B-cell lymphoma",
                            "1820 Diffuse, large B-cell lymphoma, germinal center B-cell type",
                            "1821 Diffuse, large B-cell lymphoma, activated B-cell type",
                            "1822 Primary cutaneous DLBCL, leg type",
                            "1823 EBV+ DLBCL, NOS"
                          ),
                          "High-grade B-cell lymphoma, NOS" = c(
                            "1830 High-grade B-cell lymphoma, NOS"
                          ),
                          "HGBCL/DLBCL with MYC arrangements" = c(
                            "1831 High-grade B-cell lymphoma, with MYC and BCL2 and/or BCL6 rearrangements",
                            "1837 Diffuse large B-cell lymphoma/ high grade B-cell lymphoma with MYC and BCL6 rearrangements",
                            "1838 Diffuse large B-cell lymphoma/ high grade B-cell lymphoma with MYC, BCL2, and BCL6 rearrangements"
                          ),
                          "Follicular Lymphoma Grade 3B" = c(
                            "163 Follicular, predominantly large cell (grade IIIB)"
                          )
    )
  )

#Filter out age younger than 18
df_cart_f<-df_cart_f %>% filter(age >= 18)

#Filter out embargoed data
df_cart_f<-df_cart_f %>% filter(cntembargo == "not embargoed")

#Filter out response before 1 year
df_cart_f<-df_cart_f %>% filter(intpfs>=365)

#Filter out progressive disease and not assessed from best response
df_cart_f<-df_cart_f %>% filter(!is.na(ct_lympetbresp_1y) & !ct_lympetbresp_1y %in% c("5 Progressive disease (PD)","6 Not assessed"))

#~ ~ ~ Create Table 1 for filtered cohort ~ ~ ~
tableone<-CreateTableOne(vars = vars, factorVars = varsToFactor, data=df_cart_f,
                         test = FALSE, includeNA = TRUE, addOverall = TRUE)
write.csv(print(tableone, varLabels=TRUE, useLabels=TRUE, showAllLevels=TRUE), file="table1.csv")

#~ ~ ~ Create Table 1 for excluded cohort ~ ~ ~
df_cart_exc<-df_cart %>% anti_join(df_cart_f, by="row_id")
tableone<-CreateTableOne(vars = vars, factorVars = varsToFactor, data=df_cart_exc,
                         test = FALSE, includeNA = TRUE, addOverall = TRUE)
write.csv(print(tableone, varLabels=TRUE, useLabels=TRUE, showAllLevels=TRUE), file="table1exccomp.csv")

#~ ~ ~ Transform response variable for PFS ~ ~ ~
df_cart_f$intpfs_transform<-(df_cart_f$intpfs-365)/365

#~ ~ ~ Transform response variable for OS ~ ~ ~
df_cart_f$inctsurv_transform<-(df_cart_f$inctsurv-365)/365

#~ ~ ~ Transform event variable for PFS ~ ~ ~
df_cart_f$pfs_transform<-as.numeric(substr(as.character(df_cart_f$pfs),1,1))
df_cart_f$pfsstat_transform<-as.numeric(substr(as.character(df_cart_f$pfsstat),1,1))
df_cart_f$pfsstat_transform[df_cart_f$pfsstat_transform==2]<-1
df_cart_f$pfsstat_transform[df_cart_f$pfsstat_transform==3]<-2

#~ ~ ~ Transform event variable for OS ~ ~ ~
df_cart_f$dead_transform<-as.numeric(substr(as.character(df_cart_f$dead),1,1))

#~ ~ ~ Transform event variable for relapse ~ ~ ~
df_cart_f$rel_transform<-as.numeric(substr(as.character(df_cart_f$rel),1,1))

#~ ~ ~ Define colors for future plots ~ ~ ~
colors<-c("red", "blue", "darkgreen")

#~ ~ ~ Overall PFS ~ ~ ~
#Kaplan-Meier
km_pfs_overall<-survfit(Surv(intpfs_transform,pfs_transform)~1,data=df_cart_f)

#Obtain estimates at 1, 2, and 5 years post landmark
summary(km_pfs_overall, times=c(1,2,5))

#Generate plot
pfs_overall_plot<-ggsurvplot(
  fit = km_pfs_overall,
  data = df_cart_f,
  size = 1,
  censor = FALSE,
  conf.int = FALSE,
  title = "Progression-Free Survival (PFS) in Landmark Cohort (N = 1,416)",
  xlab = "Time Since 1-Year Landmark (Years)",
  ylab = "Progression Free Survival",
  break.x.by=0.5,
  xlim = c(0,7.5),
  ylim = c(0.00,1),
  legend = "none",
  legend.title="",
  risk.table = TRUE,
  risk.table.y.text = FALSE,
  ggtheme = theme_classic(base_size=12)+
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, lineheight = 13),
      axis.title = element_text(face = "bold"),
    )
)
pfs_overall_plot

#~~~~~~~ PFS by Product ~~~~~~~
#Kaplan-Meier
km_pfs_product<-survfit(Surv(intpfs_transform,pfs_transform)~product,data=df_cart_f)

#Log-Rank Test Comparison for PFS Kaplan-Meier by Product
lr_pfs_product<-survdiff(Surv(intpfs_transform, pfs_transform)~product, data=df_cart_f)
print(lr_pfs_product)

#Generate Plot
pfs_product_plot<-ggsurvplot(
  fit = km_pfs_product,
  data = df_cart_f,
  size = 1,
  palette = colors,
  censor = FALSE,
  conf.int = FALSE,
  title = "Progression-Free Survival (PFS) by CAR-T Product",
  xlab = "Time Since 1-Year Landmark (Years)",
  ylab = "Progression Free Survival",
  break.x.by=0.5,
  xlim = c(0,7.5),
  ylim = c(0.00,1),
  legend.title="",
  legend.labs=c("Breyanzi", "Kymriah", "Yescarta"),
  legend = c(0.10,0.12),
  risk.table = TRUE,
  risk.table.y.text = FALSE,
  ggtheme = theme_classic(base_size=12)+
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, lineheight = 13),
      axis.title = element_text(face = "bold"),
    )
)
pfs_product_plot
#~~~~~~~ Overall OS ~~~~~~~~
#Kaplan-Meier
km_os_overall<-survfit(Surv(inctsurv_transform,dead_transform)~1,data=df_cart_f)

#Obtain estimates at 1, 2, and 5 years post-landmark
summary(km_os_overall, times=c(1,2,5))

#Generate plot
os_overall_plot<-ggsurvplot(
  fit = km_os_overall,
  data = df_cart_f,
  color = "darkblue",
  size = 1,
  censor = FALSE,
  conf.int = FALSE,
  title = " Overall Survival (OS) in Landmark Cohort (N = 1,416)",
  xlab = "Time Since 1-Year Landmark (Years)",
  ylab = "Overall Survival",
  break.x.by=0.5,
  xlim = c(0,7.5),
  ylim = c(0.00,1),
  legend = "none",
  legend.title="",
  risk.table = TRUE,
  risk.table.y.text = FALSE,
  ggtheme = theme_classic(base_size=12)+
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, lineheight = 13),
      axis.title = element_text(face = "bold"),
    )
)
os_overall_plot

#~ ~ ~ OS by Product ~ ~ ~
#Kaplan-Meier
km_os_product<-survfit(Surv(inctsurv_transform,dead_transform)~product,data=df_cart_f)

#Log-Rank Test Comparison for OS Kaplan-Meier by Product
lr_os_product<-survdiff(Surv(inctsurv_transform, dead_transform)~product, data=df_cart_f)
print(lr_os_product)

#Generate plot
os_product_plot<-ggsurvplot(
  fit = km_os_product,
  data = df_cart_f,
  size = 1,
  palette = colors,
  censor = FALSE,
  conf.int = FALSE,
  title = "Overall Survival (OS) by CAR-T Product",
  xlab = "Time Since 1-Year Landmark (Years)",
  ylab = "Overall Survival Probability (%)",
  break.x.by=0.5,
  xlim = c(0,7.5),
  ylim = c(0.00, 1),
  legend.title="",
  legend.labs=c("Breyanzi", "Kymriah", "Yescarta"),
  legend = c(0.10,0.12),
  legend.size = 1,
  risk.table = TRUE,
  risk.table.y.text = FALSE,
  ggtheme = theme_classic(base_size=12)+
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, lineheight = 13),
      axis.title = element_text(face = "bold"),
    )
)
os_product_plot

#~~~~~~ Cumulative Incidence Curves by Product ~~~~~~
ci_rel<-tidycmprsk::cuminc(Surv(intpfs_transform, as.factor(rel_transform))~product, data=df_cart_f)
print(ci_rel)

#Generate plot
ci_product_plot<-ggcuminc(ci_rel, outcome = "1", size=1,
                          ) +
  scale_color_manual(
    values  = c("red","blue","darkgreen"),
    labels = c("Breyanzi", "Kymriah", "Yescarta")
  ) +
  scale_x_continuous(
    limits = c(0,7.5),
    breaks = seq(0,7.5,by=0.5)
  ) + 
  scale_y_continuous(
    limits = c(0,1),
    breaks = seq(0,1, by=0.10)
  )+
  labs(
    title = "Cumulative Incidence of Relapse by Product",
    x = "Time Since 1-Year Landmark (Years)",
    y = "Cumulative Incidence",
    subtitle = "With Death as a Competing Risk"
  ) +
  theme_classic(base_size=12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, lineheight = 13),
    legend.position = c(0.9,0.1),
    axis.title = element_text(face = "bold"),
    plot.subtitle = element_text(face="italic", hjust = 0.5)
  )
ci_product_plot

#~~~~~~ Cumulative Incidence Curve Overall ~~~~~~
ci_rel_o<-tidycmprsk::cuminc(Surv(intpfs_transform, as.factor(rel_transform))~1, data=df_cart_f)
print(ci_rel_o)

#Extract estimates at 1,2,5 years post-landmark
tidy(ci_rel_o, times = c(1,2,5))

#Generate plot
ci_overall_plot<-ggcuminc(ci_rel_o,color = "darkblue", outcome = "1", size=1,
) +
  scale_x_continuous(
    limits = c(0,7.5),
    breaks = seq(0,7.5,by=0.5),
    labels = function(x) sub("\\.0$", "", sprintf("%.1f",x))
  ) + 
  scale_y_continuous(
    limits = c(0,1),
    breaks = seq(0,1, by=0.25)
  )+
  labs(
    title = "Cumulative Incidence of Relapse in Landmark Cohort (N = 1,416)",
    x = "Time Since 1-Year Landmark (Years)",
    y = "Cumulative Incidence",
    subtitle = "With Death as a Competing Risk"
  ) +
  theme_classic(base_size=12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, lineheight = 13),
    legend.position = c(0.9,0.1),
    axis.title = element_text(face = "bold"),
    plot.subtitle = element_text(face="italic", hjust = 0.5),
  )

ci_overall_plot

#~ ~ ~ Data manipulation prior to modeling ~ ~ ~
#Transform relapse variable
df_cart_f$rel_model<-ifelse(df_cart_f$rel_transform == 1, 1,
                            ifelse(df_cart_f$rel_transform %in% c(0,2),0,NA))

#Drop unused levels
df_cart_f<-droplevels(df_cart_f)

#~ ~ ~ Create version of the dataset for modeling purposes ~ ~ ~
df_cart_fm<-dplyr::select(df_cart_f, c("did","product", "rel_model","intpfs_transform","age", "cntembargo", "center_country_tx", "sex", "intdxct", "ct_lymstgdx", "ct_exnodpr","ntx_any","crs_fever_any", "ct_elevatedldhpr","ct_subdis", "numlinestherapy", "ct_treatbg", "ct_lympetbresp_1y"))
df_cart_fm<-droplevels(df_cart_fm)
colSums(is.na(df_cart_fm))

#~ ~ ~ Handling missing data as category for analysis ~ ~ ~
#ct_lymstgdx
df_cart_fm$ct_lymstgdx <- fct_recode(df_cart_fm$ct_lymstgdx,
                                     "99 Missing" = "8 Unknown")
df_cart_fm$ct_lymstgdx <- fct_na_value_to_level(df_cart_fm$ct_lymstgdx, level = "99 Missing")

#ct_exnodpr
df_cart_fm$ct_exnodpr <- fct_recode(df_cart_fm$ct_exnodpr,
                                     "99 Missing" = "8 Unknown")
df_cart_fm$ct_exnodpr <- fct_na_value_to_level(df_cart_fm$ct_exnodpr, level = "99 Missing")

#numlinestherapy
df_cart_fm$numlinestherapy <- fct_na_value_to_level(df_cart_fm$numlinestherapy, level = "99 Missing")

#ct_treatbg
df_cart_fm$ct_treatbg <- fct_na_value_to_level(df_cart_fm$ct_treatbg, level = "99 Missing")

#ct_elevatedldhpr
df_cart_fm$ct_elevatedldhpr<- fct_na_value_to_level(df_cart_fm$ct_elevatedldhpr, level = "99 Missing")

#~ ~ ~ Fit full model ~ ~ ~
full_model_fit<-coxph(Surv(intpfs_transform, rel_model)~age  + sex + crs_fever_any + numlinestherapy + ntx_any + ct_exnodpr +product + ct_lymstgdx + ct_subdis + ct_lympetbresp_1y + ct_elevatedldhpr + intdxct + ct_treatbg, data=df_cart_fm)

#~ ~ ~ Test for assumption of proportional hazards ~ ~ ~
cox.zph(full_model_fit, terms = TRUE)

#~ ~ ~ Stepwise Variable Selection to Obtain Reduced Model ~ ~ ~
library(MASS)
step_wise<-stepAIC(full_model_fit, direction = "both", trace = FALSE)

#~ ~ ~ Test for assumption of proportional hazards ~ ~ ~
cox.zph(step_wise)

#~ ~ ~ Obtain global p-values for multi-category variables~ ~ ~
library(car)
car::Anova(full_model_fit, type="II", test.statistic="Wald")

#~ ~ ~ Generate Regression Tables ~ ~ ~

models<-list(
  "Full Model"=full_model_fit,
  "Reduced Model"=step_wise
)


modelsummary(
  models,
  fmt = 2,
  stars = c("*"=0.05,"**"=0.01,"***"=0.001),
  notes = "Std. Errors in Parentheses",
  title = "Table: Baseline Model Regression Results",
  estimate = "{estimate}, [{conf.low}, {conf.high}] {p.value}{stars}",
  statistic = NULL,
  gof_omit = "F|BIC|Log.Lik.|RMSE",
  exponentiate = TRUE
)


#~ ~ ~ Comparison of full to reduced model ~ ~ ~
anova(full_model_fit, step_wise, test="LRT")
