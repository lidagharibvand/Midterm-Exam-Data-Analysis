# ------------------------------------------------------------------
# Data Analysis 471: Midterm Exam, R code (HALO dataset)
# Data: halo.xlsx (HALO trial, one row per participant)
#
# Put halo.xlsx in your RStudio project folder (the same folder as
# this script), then run the script one question at a time.
# Paste every table and graph into your Word document.
#
# Every line is labeled with the question and part it answers,
# for example "1.a" means Question 1, part a.
# ------------------------------------------------------------------

# ---- SETUP: load the data (needed before every question) ---------

# install.packages("readxl")                    # SETUP: run once only, if readxl is not installed
library(readxl)                                  # SETUP: load the package that reads Excel files

halo <- as.data.frame(read_excel("halo.xlsx"))   # SETUP: read the data into R
names(halo) <- tolower(names(halo))              # SETUP: lowercase names (BMI -> bmi, LDL0 -> ldl0)

dim(halo)                                        # SETUP: 2000 participants, 38 variables
names(halo)                                      # SETUP: list the variable names
colSums(is.na(halo))                             # SETUP: count missing values in each column


# ==================================================================
# QUESTION 1. Outliers and impossible values: age
# ==================================================================

# ---- 1.a: age exactly as it is in the file -----------------------
summary(halo$age)                                # 1.a: min, Q1, median, mean, Q3, max
mean(halo$age)                                   # 1.a: mean age (820 still included)
sd(halo$age)                                     # 1.a: standard deviation (820 still included)
sum(!is.na(halo$age))                            # 1.a: number of participants with an age

# ---- 1.b: histogram and boxplot BEFORE screening -----------------
# 1.b: histogram of age before screening
hist(halo$age, main = "Histogram of age (before screening)",
     xlab = "Age (years)", ylab = "Number of participants",
     col = "lightblue", border = "black")

# 1.b: boxplot of age before screening
boxplot(halo$age, main = "Boxplot of age (before screening)",
        ylab = "Age (years)", col = "lightgreen")

boxplot.stats(halo$age)$out                      # 1.b: values drawn as separate points

# ---- 1.c: find the participant with the impossible value ---------
halo[halo$age > 120, c("id", "age", "female", "sbp0")]  # 1.c: who has an age above 120?

dev2 <- (halo$age - mean(halo$age))^2            # 1.c: squared deviation of each age from the mean
max(dev2) / sum(dev2)                            # 1.c: share of all variation from this one value

# ---- 1.d: set the impossible value to NA -------------------------
halo$age[halo$age > 120] <- NA                   # 1.d: one rule, one line

# ---- 1.e: age AFTER screening ------------------------------------
summary(halo$age)                                # 1.e: summary after screening
mean(halo$age, na.rm = TRUE)                     # 1.e: mean after screening
sd(halo$age, na.rm = TRUE)                       # 1.e: standard deviation after screening
sum(!is.na(halo$age))                            # 1.e: participants with an age (now 1,999)

# 1.e: histogram of age after screening
hist(halo$age, main = "Histogram of age (after screening)",
     xlab = "Age (years)", ylab = "Number of participants",
     col = "lightblue", border = "black")

# 1.e: boxplot of age after screening
boxplot(halo$age, main = "Boxplot of age (after screening)",
        ylab = "Age (years)", col = "lightgreen")

boxplot.stats(halo$age)$out                      # 1.e and 1.f: points still beyond the whiskers
table(boxplot.stats(halo$age)$out)               # 1.e and 1.f: how many, and which ages

# ---- 1.f: before-and-after table ---------------------------------
age_raw <- as.data.frame(read_excel("halo.xlsx"))$age   # 1.f: original ages, 820 included

# 1.f: a small function that returns the eight statistics for the table
describe_age <- function(x) {
  round(c(n = sum(!is.na(x)), mean = mean(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE), median = median(x, na.rm = TRUE),
          Q1 = unname(quantile(x, 0.25, na.rm = TRUE)),
          Q3 = unname(quantile(x, 0.75, na.rm = TRUE)),
          min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE)), 2)
}

rbind(before = describe_age(age_raw),            # 1.f: before screening
      after  = describe_age(halo$age))           # 1.f: after screening


# ==================================================================
# QUESTION 2. Table of baseline characteristics
# ==================================================================

# ---- 2.a: screen BMI first (age was screened in Question 1) -------
summary(halo$bmi)                                # 2.a: look at the minimum BMI
halo[halo$bmi <= 0, c("id", "bmi")]              # 2.a: who has a BMI of 0?
halo$bmi[halo$bmi <= 0] <- NA                    # 2.a: set the impossible BMI to NA
summary(halo$bmi)                                # 2.a: check BMI after screening

# ---- 2.b: two helper functions, so every row is made the same way -
# 2.b: mean ± SD and n, for continuous variables
mean_sd <- function(x) {
  sprintf("%.1f ± %.1f  (n = %d)",
          mean(x, na.rm = TRUE), sd(x, na.rm = TRUE), sum(!is.na(x)))
}
# 2.b: frequency (percentage), for categorical variables
n_pct <- function(x) {
  counts <- table(x)
  pct    <- 100 * counts / sum(counts)
  paste0(names(counts), ": ", counts, " (", sprintf("%.1f", pct), "%)",
         collapse = "; ")
}

# ---- 2.b: one line for each row of the table ----------------------
mean_sd(halo$age)                                # 2.b: row "Age, years"
mean_sd(halo$bmi)                                # 2.b: row "Body mass index, kg/m2"
mean_sd(halo$weight0)                            # 2.b: row "Baseline weight, kg"
mean_sd(halo$weight12)                           # 2.b: row "Weight at 12 months, kg"
mean_sd(halo$glucose)                            # 2.b: row "Fasting glucose, mg/dL"
mean_sd(halo$dbp0)                               # 2.b: row "Diastolic blood pressure, mmHg"
mean_sd(halo$ldl0)                               # 2.b: row "LDL cholesterol at baseline, mg/dL"
n_pct(halo$diabetes)                             # 2.b: row "Diagnosed diabetes" (0 = no, 1 = yes)
n_pct(halo$statins)                              # 2.b: row "Statin use at enrollment" (0 = no, 1 = yes)

# ---- 2.b: the same results the long way (optional) ----------------
mean(halo$bmi, na.rm = TRUE)                     # 2.b: mean BMI
sd(halo$bmi, na.rm = TRUE)                       # 2.b: SD of BMI
table(halo$statins)                              # 2.b: count of statin users
round(100 * prop.table(table(halo$statins)), 1)  # 2.b: percent of statin users

# 2.b: the whole table at once, ready to copy into Word
table1 <- data.frame(
  Variable = c("Age (years)", "BMI (kg/m^2)", "Baseline weight (kg)",
               "Weight at 12 months (kg)", "Fasting glucose (mg/dL)",
               "Diastolic BP (mmHg)", "LDL cholesterol (mg/dL)",
               "Diabetes", "Statin use"),
  Summary = c(mean_sd(halo$age), mean_sd(halo$bmi), mean_sd(halo$weight0),
              mean_sd(halo$weight12), mean_sd(halo$glucose),
              mean_sd(halo$dbp0), mean_sd(halo$ldl0),
              n_pct(halo$diabetes), n_pct(halo$statins)))
print(table1, right = FALSE, row.names = FALSE)  # 2.b: print the table


# ==================================================================
# QUESTION 3. Boxplots of LDL cholesterol
# ==================================================================

# ---- 3.a: boxplot of LDL and its five numbers -------------
# 3.a: boxplot of baseline LDL
boxplot(halo$ldl0, main = "Boxplot of baseline LDL cholesterol",
        ylab = "LDL cholesterol (mg/dL)", col = "lightgreen")

summary(halo$ldl0)                               # 3.a: min, Q1, median, mean, Q3, max
quantile(halo$ldl0, probs = c(0, 0.25, 0.50, 0.75, 1))  # 3.a: min, Q1, Q2, Q3, max
IQR(halo$ldl0)                                   # 3.a: interquartile range

# ---- 3.b: LDL by statin use -------------------------------
# 3.b: label the 0/1 statin codes
halo$statins_f <- factor(halo$statins, levels = c(0, 1),
                         labels = c("No statin", "Statin"))
table(halo$statins, halo$statins_f)              # 3.b: check the labels match the codes

# 3.b: side-by-side boxplots of LDL by statin use
boxplot(ldl0 ~ statins_f, data = halo,
        main = "Baseline LDL cholesterol by statin use",
        xlab = "Statin use at enrollment", ylab = "LDL cholesterol (mg/dL)",
        col = c("lightgreen", "lightblue"))

tapply(halo$ldl0, halo$statins_f, summary)       # 3.b: five-number summary in each group
tapply(halo$ldl0, halo$statins_f, IQR)           # 3.b and 3.c: IQR (spread) in each group


# ==================================================================
# QUESTION 4. Graphs for categorical and count variables
# ==================================================================

# 4.a: put the four chest pain types in clinical order
halo$angina_f <- factor(halo$angina,
                        levels = c("None", "Non-anginal",
                                   "Atypical", "Typical"))
angina_counts <- table(halo$angina_f)            # 4.a: count in each category
angina_counts                                    # 4.a: print the counts
angina_pct <- round(100 * prop.table(angina_counts), 1)  # 4.a: percent in each category
angina_pct                                       # 4.a: print the percents

# 4.a: pie chart, each slice labeled with its percent
pie(angina_counts,
    labels = paste0(names(angina_counts), " (", angina_pct, "%)"),
    main = "Chest pain type at enrollment",
    col = c("steelblue", "lightgreen", "khaki", "firebrick"))

# 4.a: bar chart
bars <- barplot(angina_counts, ylim = c(0, 750),
                main = "Chest pain type at enrollment",
                xlab = "Chest pain type", ylab = "Number of participants",
                col = "steelblue")
text(bars, angina_counts, labels = angina_counts, pos = 3)  # 4.a: write each count above its bar

# ---- 4.c and 4.d: a count variable -------------------------------
vessel_counts <- table(halo$vessels)             # 4.c: count for 0, 1, 2 and 3 vessels

# 4.c: frequency table with counts and percentages
cbind(n = vessel_counts,
      percent = round(100 * vessel_counts / sum(vessel_counts), 1))

# 4.c: bar chart, one bar for each number of vessels
bars <- barplot(vessel_counts, ylim = c(0, 1050),
                main = "Diseased coronary vessels on imaging",
                xlab = "Number of diseased vessels",
                ylab = "Number of participants", col = "steelblue")
text(bars, vessel_counts, labels = vessel_counts, pos = 3)  # 4.c: write each count above its bar
