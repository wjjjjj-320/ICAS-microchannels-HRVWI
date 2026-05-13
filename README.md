#Baseline description




library(readxl)
library(dplyr)
library(tableone)
library(openxlsx)
data <- read_excel("./example_data/example_input.csv”)
data$microchannel_presence <- as.factor(data$microchannel_presence)

cat_vars <- c(
"gender","hp","dm","hlp","chd","smoking","drinking",
"index_event_type","culprit_vessel","culprit_vesselnew",
"thombus","cholesterol_crystal","calcification",
"macrophage","plaque_rupture"
)

cont_vars <- c(
"age","LDL","nEI","Enhancement_ratio","Contrast_ratio_change","stenosis"
)

data[cat_vars] <- lapply(data[cat_vars], as.factor)


normality_test <- lapply(cont_vars, function(v) {
x <- data[[v]]
x <- x[!is.na(x)]
if (length(x) < 3 || length(x) > 5000) {
return(list(p.value = NA_real_, note = "Shapiro not applicable (n<3 or n>5000)"))
}
out <- shapiro.test(x)
list(p.value = out$p.value, note = NA_character_)
})
names(normality_test) <- cont_vars

normality_df <- data.frame(
Variable = cont_vars,
Shapiro_P = sapply(normality_test, function(z) z$p.value),
Note = sapply(normality_test, function(z) z$note),
stringsAsFactors = FALSE
)

nonnormal_vars <- c("age","LDL","nEI","Contrast_ratio_change","stenosis")

table1 <- CreateTableOne(
vars = c(cont_vars, cat_vars),
strata = "microchannel_presence",
data = data,
factorVars = cat_vars,
addOverall = TRUE
)

table1_mat <- print(
table1,
nonnormal = nonnormal_vars,
showAllLevels = TRUE,
quote = FALSE,
noSpaces = TRUE,
printToggle = FALSE
)

table1_df <- as.data.frame(table1_mat, stringsAsFactors = FALSE)
table1_df <- cbind(Variable = rownames(table1_df), table1_df)
rownames(table1_df) <- NULL

out_file <- "Table1_microchannel_overall_and_groups.xlsx"

wb <- createWorkbook()

addWorksheet(wb, "Table1")
writeData(wb, "Table1", table1_df)

addWorksheet(wb, "Normality_Shapiro")
writeData(wb, "Normality_Shapiro", normality_df)

saveWorkbook(wb, out_file, overwrite = TRUE)

cat("Done! Excel saved to: ", normalizePath(out_file), "\n")



#Descriptions of nEI for different groups of people
library(readxl)
library(dplyr)
library(openxlsx)

data <- read_excel("./example_data/example_input.csv")

categorical_vars <- c(
"gender", "hp", "dm", "hlp", "chd", "smoking", "drinking",
"index_event_type", "culprit_vesselnew",
"thombus", "cholesterol_crystal", "calcification",
"macrophage"
)

continuous_vars <- c("age", "LDL", "stenosis")

outcome <- "nEI"

wb <- createWorkbook()

fmt_median_iqr <- function(x, digits = 3) {
x <- x[!is.na(x)]
if (length(x) == 0) return(NA_character_)
  
med <- median(x)
q1 <- quantile(x, 0.25, names = FALSE)
q3 <- quantile(x, 0.75, names = FALSE)
  
sprintf(
paste0("%.", digits, "f [%.", digits, "f-%.", digits, "f]"),
med, q1, q3
)
}

desc_cat <- lapply(categorical_vars, function(var) {
data %>%
filter(!is.na(.data[[var]])) %>%
group_by(.data[[var]]) %>%
summarise(
N = sum(!is.na(.data[[outcome]])),
`nEI, median [Q1-Q3]` = fmt_median_iqr(.data[[outcome]], digits = 3),
.groups = "drop"
) %>%
mutate(Variable = var) %>%
rename(Stratum = 1) %>%
select(Variable, Stratum, N, `nEI, median [Q1-Q3]`)
}) %>% bind_rows()

addWorksheet(wb, "Categorical_Strata")
writeData(wb, "Categorical_Strata", desc_cat)

desc_cont <- lapply(continuous_vars, function(var) {
median_val <- median(data[[var]], na.rm = TRUE)
  
data %>%
filter(!is.na(.data[[var]])) %>%
mutate(
Stratum = ifelse(
.data[[var]] <= median_val,
paste0(var, "_low"),
paste0(var, "_high")
)
) %>%
group_by(Stratum) %>%
summarise(
N = sum(!is.na(.data[[outcome]])),
`nEI, median [Q1-Q3]` = fmt_median_iqr(.data[[outcome]], digits = 3),
.groups = "drop"
) %>%
mutate(Variable = var) %>%
select(Variable, Stratum, N, `nEI, median [Q1-Q3]`)
}) %>% bind_rows()

addWorksheet(wb, "Continuous_Strata")
writeData(wb, "Continuous_Strata", desc_cont)

saveWorkbook(wb, "nEI_descriptive_SCI_table.xlsx", overwrite = TRUE)

cat("Done! Excel saved to: ", normalizePath("nEI_descriptive_SCI_table.xlsx"), "\n")


# univariate linear regression

get_lm_results <- function(var){
formula <- as.formula(paste(outcome, "~", var))
fit <- lm(formula, data = data)
  
tidy_fit <- broom::tidy(fit, conf.int = FALSE)
ci <- stats::confint(fit)
  

beta <- tidy_fit$estimate[2]
p_val <- tidy_fit$p.value[2]
ci_lower <- ci[2, 1]
ci_upper <- ci[2, 2]
r2 <- summary(fit)$r.squared
  
data.frame(
variable = var,
beta = beta,
CI_lower = ci_lower,
CI_upper = ci_upper,
p_value = p_val,
R2 = r2
)
}
lm_results <- lapply(c(categorical_vars, continuous_vars), get_lm_results) %>%
bind_rows()

addWorksheet(wb, "Univariate_LM")
writeData(wb, "Univariate_LM", lm_results)

saveWorkbook(wb, "./example_data/example_input.xlsx", overwrite = TRUE)

#nEI的分布描述

library(readxl)
library(ggplot2)
library(dplyr)
library(grid) # unit()


file_path <- "./example_data/example_input.csv”
data <- read_excel(file_path, sheet = "Sheet2")


data$microchannel_presence <- factor(
data$microchannel_presence,
levels = c(0, 1),
labels = c("microchannel (-)", "microchannel (+)")
)


data_plot <- data %>%
filter(!is.na(nEI), !is.na(microchannel_presence))


cat("Total sample size =", nrow(data_plot), "\n")

shapiro_result <- shapiro.test(data_plot$nEI)
print(shapiro_result)

if (shapiro_result$p.value > 0.05) {
cat("Conclusion: nEI approximately follows a normal distribution (Shapiro-Wilk test: P =",
round(shapiro_result$p.value, 4), ")\n")
} else {
cat("Conclusion: nEI does not follow a normal distribution (Shapiro-Wilk test: P =",
round(shapiro_result$p.value, 4), ")\n")
}

p_qq <- ggplot(data_plot, aes(sample = nEI)) +
stat_qq(size = 2, alpha = 0.8) +
stat_qq_line(linewidth = 0.8, color = "red") +
labs(
title = "Q-Q plot of nEI",
x = "Theoretical Quantiles",
y = "Sample Quantiles"
) +
theme_classic(base_size = 14) +
theme(
plot.title = element_text(hjust = 0.5, face = "bold"),
axis.title = element_text(face = "bold"),
axis.text = element_text(color = "black"),
axis.line = element_line(color = "black", linewidth = 0.8),
axis.ticks = element_line(color = "black", linewidth = 0.8)
)

print(p_qq)

ggsave(
filename = "nEI_QQ_plot.tiff",
plot = p_qq,
width = 5.5,
height = 5,
dpi = 600,
units = "in",
compression = "lzw"
)

ggsave(
filename = "nEI_QQ_plot.png",
plot = p_qq,
width = 5.5,
height = 5,
dpi = 600,
units = "in"
)


p_main <- ggplot(data_plot, aes(x = microchannel_presence, y = nEI)) +
  
geom_violin(
width = 0.9,
fill = "grey90",
color = "black",
linewidth = 0.6,
trim = FALSE
) +
  
geom_boxplot(
width = 0.16,
fill = "white",
color = "black",
linewidth = 0.7,
outlier.shape = NA
) +
  
geom_jitter(
width = 0.08,
height = 0,
size = 1.8,
alpha = 0.7,
shape = 16,
color = "black"
) +
  
labs(
x = NULL,
y = "nEI"
) +
  
theme_classic(base_size = 14) +
theme(
axis.title.y = element_text(size = 14, face = "bold"),
axis.text.x = element_text(size = 12, face = "bold", color = "black"),
axis.text.y = element_text(size = 12, color = "black"),
axis.line = element_line(linewidth = 0.8, color = "black"),
axis.ticks = element_line(linewidth = 0.8, color = "black"),
axis.ticks.length = unit(0.18, "cm"),
plot.margin = margin(10, 10, 10, 10)
)

print(p_main)

ggsave(
filename = "nEI_microchannel_violin_box_jitter.tiff",
plot = p_main,
width = 6,
height = 5,
dpi = 600,
units = "in",
compression = "lzw"
)

ggsave(
filename = "nEI_microchannel_violin_box_jitter.png",
plot = p_main,
width = 6,
height = 5,
dpi = 600,
units = "in"
)

cat("All analyses completed successfully.\n")




#Univariable and multivariable linear regression
library(readxl)
library(dplyr)
library(broom)
library(openxlsx)
library(lmtest)
library(sandwich)
library(robustbase)
library(quantreg)
library(e1071)


data <- read_excel("./example_data/example_input.csv")


if ("gender" %in% names(data)) data$gender <- as.factor(data$gender)


data <- data %>%
mutate(
microchannel_presence_raw = microchannel_presence,
microchannel_presence = as.character(microchannel_presence),
microchannel_presence = ifelse(microchannel_presence %in% c("1","Yes","YES","yes","+","positive","Positive"), 1,
ifelse(microchannel_presence %in% c("0","No","NO","no","-","negative","Negative"), 0, NA)),
microchannel_presence = as.numeric(microchannel_presence),
microchannel_group = factor(microchannel_presence, levels = c(0,1),
labels = c("microchannel(-)","microchannel(+)"))
)


factor_covars <- c("hp","dm","chd","culprit_vesselnew","thombus",
"cholesterol_crystal","macrophage","calcification")
for (v in factor_covars) {
if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
}

need_vars <- c("nEI","microchannel_presence","microchannel_group",
"age","gender","hp","dm","chd","LDL",
"culprit_vesselnew","thombus","cholesterol_crystal","macrophage","calcification")
need_vars <- intersect(need_vars, names(data))

dat <- data %>%
select(all_of(need_vars)) %>%
filter(!is.na(nEI), !is.na(microchannel_presence))


nei <- dat$nEI
nei_non_na <- nei[!is.na(nei)]

shapiro_p <- if (length(nei_non_na) >= 3 && length(nei_non_na) <= 5000) {
shapiro.test(nei_non_na)$p.value
} else {
NA_real_
}

dist_df <- data.frame(
n = length(nei_non_na),
mean = mean(nei_non_na),
sd = sd(nei_non_na),
median = median(nei_non_na),
IQR = IQR(nei_non_na),
skewness = e1071::skewness(nei_non_na, na.rm = TRUE, type = 2),
kurtosis = e1071::kurtosis(nei_non_na, na.rm = TRUE, type = 2),
shapiro_p = shapiro_p
)


extract_lm_micro <- function(fit, term = "microchannel_presence") {
s <- summary(fit)
r2 <- s$r.squared
b <- broom::tidy(fit, conf.int = TRUE)
out <- b %>%
filter(term == !!term) %>%
transmute(
term = term,
beta = estimate,
conf.low = conf.low,
conf.high = conf.high,
p.value = p.value,
R2 = r2
)
out
}


extract_lm_micro_HC3 <- function(fit, term = "microchannel_presence") {
vc <- sandwich::vcovHC(fit, type = "HC3")
ct <- lmtest::coeftest(fit, vcov. = vc)
beta <- unname(ct[term, "Estimate"])
se <- unname(ct[term, "Std. Error"])
p <- unname(ct[term, "Pr(>|t|)"])
 
ci_low <- beta - 1.96 * se
ci_high <- beta + 1.96 * se
r2 <- summary(fit)$r.squared
  
data.frame(
term = term,
beta = beta,
conf.low = ci_low,
conf.high = ci_high,
p.value = p,
R2 = r2
)
}


fit_uni <- lm(nEI ~ microchannel_presence, data = dat)
res_uni <- extract_lm_micro(fit_uni)

res_uni_hc3 <- extract_lm_micro_HC3(fit_uni)


fit_m1 <- lm(nEI ~ microchannel_presence + age + gender, data = dat)
fit_m2 <- lm(nEI ~ microchannel_presence + age + gender + hp + dm + chd + LDL, data = dat)
fit_m3 <- lm(nEI ~ microchannel_presence + age + gender + hp + dm + chd + LDL +
culprit_vesselnew + thombus + cholesterol_crystal + macrophage + calcification, data = dat)

res_m1 <- extract_lm_micro(fit_m1)
res_m2 <- extract_lm_micro(fit_m2)
res_m3 <- extract_lm_micro(fit_m3)

res_m1_hc3 <- extract_lm_micro_HC3(fit_m1)
res_m2_hc3 <- extract_lm_micro_HC3(fit_m2)
res_m3_hc3 <- extract_lm_micro_HC3(fit_m3)

main_ols <- bind_rows(
cbind(Model = "Univariable", res_uni),
cbind(Model = "Model 1 (age + sex)", res_m1),
cbind(Model = "Model 2 (+ hp dm chd LDL)", res_m2),
cbind(Model = "Model 3 (+ imaging/pathology)", res_m3)
)

main_ols_hc3 <- bind_rows(
cbind(Model = "Univariable (HC3)", res_uni_hc3),
cbind(Model = "Model 1 (HC3)", res_m1_hc3),
cbind(Model = "Model 2 (HC3)", res_m2_hc3),
cbind(Model = "Model 3 (HC3)", res_m3_hc3)
)

sens_list <- list()

if (all(dat$nEI > 0, na.rm = TRUE)) {
fit_log_m3 <- lm(log(nEI) ~ microchannel_presence + age + gender + hp + dm + chd + LDL +
culprit_vesselnew + thombus + cholesterol_crystal + macrophage + calcification,
data = dat)

b_log <- broom::tidy(fit_log_m3, conf.int = TRUE) %>%
filter(term == "microchannel_presence") %>%
transmute(
Model = "Log-linear (Model 3)",
beta_log = estimate,
conf.low_log = conf.low,
conf.high_log = conf.high,
p.value = p.value,
pct_change = exp(beta_log) - 1,
pct_low = exp(conf.low_log) - 1,
pct_high = exp(conf.high_log) - 1
)
sens_list$log_linear <- b_log
}

fit_rob_m3 <- lmrob(nEI ~ microchannel_presence + age + gender + hp + dm + chd + LDL +
culprit_vesselnew + thombus + cholesterol_crystal + macrophage + calcification,
data = dat)


rob_sum <- summary(fit_rob_m3)
rob_beta <- rob_sum$coefficients["microchannel_presence", "Estimate"]
rob_se <- rob_sum$coefficients["microchannel_presence", "Std. Error"]
rob_p <- rob_sum$coefficients["microchannel_presence", "Pr(>|t|)"]
rob_ci <- suppressWarnings(confint(fit_rob_m3))
rob_ci_mc <- rob_ci["microchannel_presence", ]

sens_list$robust_lm <- data.frame(
Model = "Robust LM (lmrob, Model 3)",
beta = rob_beta,
conf.low = rob_ci_mc[1],
conf.high = rob_ci_mc[2],
p.value = rob_p,
stringsAsFactors = FALSE
)


fit_rq_m3 <- rq(nEI ~ microchannel_presence + age + gender + hp + dm + chd + LDL +
culprit_vesselnew + thombus + cholesterol_crystal + macrophage + calcification,
data = dat, tau = 0.5)


rq_sum <- summary(fit_rq_m3, se = "boot", R = 1000)
rq_coef <- rq_sum$coefficients
rq_beta <- rq_coef["microchannel_presence", 1]
rq_se <- rq_coef["microchannel_presence", 2]
rq_p <- rq_coef["microchannel_presence", 4]

rq_ci_low <- rq_beta - 1.96 * rq_se
rq_ci_high <- rq_beta + 1.96 * rq_se

sens_list$quantile <- data.frame(
Model = "Quantile regression (tau=0.5, Model 3)",
beta = rq_beta,
conf.low = rq_ci_low,
conf.high = rq_ci_high,
p.value = rq_p,
stringsAsFactors = FALSE
)

sens_df <- bind_rows(sens_list)


out_file <- "LinearRegression_nEI_microchannel_results.xlsx"

wb <- createWorkbook()

addWorksheet(wb, "nEI_distribution")
writeData(wb, "nEI_distribution", dist_df)

addWorksheet(wb, "Main_OLS")
writeData(wb, "Main_OLS", main_ols)

addWorksheet(wb, "Main_OLS_HC3")
writeData(wb, "Main_OLS_HC3", main_ols_hc3)

addWorksheet(wb, "Sensitivity")
writeData(wb, "Sensitivity", sens_df)

addWorksheet(wb, "Full_coef_Model1")
writeData(wb, "Full_coef_Model1", broom::tidy(fit_m1, conf.int = TRUE))

addWorksheet(wb, "Full_coef_Model2")
writeData(wb, "Full_coef_Model2", broom::tidy(fit_m2, conf.int = TRUE))

addWorksheet(wb, "Full_coef_Model3")
writeData(wb, "Full_coef_Model3", broom::tidy(fit_m3, conf.int = TRUE))

saveWorkbook(wb, out_file, overwrite = TRUE)

cat("Done! Excel saved to: ", normalizePath(out_file), "\n")



library(ggplot2)
library(car)
library(rms)
library(performance)
diag_dir <- "lm_diagnostics_plots"
if (!dir.exists(diag_dir)) dir.create(diag_dir)
continuous_vars <- c("age", "LDL")
continuous_vars <- continuous_vars[continuous_vars %in% names(dat)]

for (v in continuous_vars) {
p <- ggplot(dat, aes_string(x = v, y = "nEI")) +
geom_point(alpha = 0.7) +
geom_smooth(method = "lm", se = TRUE, color = "blue") +
geom_smooth(method = "loess", se = TRUE, color = "red", linetype = 2) +
theme_bw() +
labs(title = paste("Scatter plot of nEI vs", v),
subtitle = "Blue: linear fit; Red: loess fit")
  
ggsave(filename = file.path(diag_dir, paste0("scatter_", v, ".png")),
plot = p, width = 6, height = 5, dpi = 300)
}
png(file.path(diag_dir, "partial_residual_plots_Model3.png"),
width = 1400, height = 700, res = 150)
car::crPlots(fit_m3)
dev.off()

wb <- loadWorkbook("LinearRegression_nEI_microchannel_results.xlsx")

addWorksheet(wb, "Residual_normality")
writeData(wb, "Residual_normality", residual_normality_df)

addWorksheet(wb, "Homoscedasticity")
writeData(wb, "Homoscedasticity", homoscedasticity_df)

addWorksheet(wb, "Influence_diagnostics")
writeData(wb, "Influence_diagnostics", influence_df)

addWorksheet(wb, "Flagged_cases")
writeData(wb, "Flagged_cases", flagged_cases)

addWorksheet(wb, "Influence_sensitivity")
writeData(wb, "Influence_sensitivity", influential_sensitivity_df)

addWorksheet(wb, "VIF")
writeData(wb, "VIF", vif_df)

addWorksheet(wb, "Diagnostic_summary")
writeData(wb, "Diagnostic_summary", diagnostic_summary)

saveWorkbook(wb, "LinearRegression_nEI_microchannel_results_with_diagnostics.xlsx", overwrite = TRUE)

cat("Done! Diagnostic results saved to: ",
normalizePath("LinearRegression_nEI_microchannel_results_with_diagnostics.xlsx"), "\n")
