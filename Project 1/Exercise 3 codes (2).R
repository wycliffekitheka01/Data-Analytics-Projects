# Install core packages for data analysis
install.packages("dplyr")    
install.packages("tidyr")    
install.packages("ggplot2")  
install.packages("readxl")   
# Loading the packages 
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
# Loading data 
df<- read_csv("C:/Users/Administrator/Downloads/HMGT400HOSPITAL(2).csv")
View(df)
# See the variable names
names(df)
# Generate new variable in df
df <- df %>%
  mutate(
    benefit = total_hosp_revenue - total_hosp_cost,
     )
# Convert herf_cat to a factor (categorical variable)
df$herf_cat <- as.factor(df$herf_cat)

# Calculate means by herf_cat
# Group the data by herf_cat
herfindahl <- group_by(df, herf_cat)

# Calculate means
summary_table <- summarize(
  herfindahl,
  bed = mean(hospital_beds, na.rm = TRUE),
  payer = mean(total_hospital_employees_on_payr, na.rm = TRUE),
  nopayer = mean(total_hospital_non_paid_workers, na.rm = TRUE),
  resident = mean(interns_and_residents, na.rm = TRUE),
  member = mean(system_member, na.rm = TRUE),
  cost = mean(total_hosp_cost, na.rm = TRUE),
  revenue = mean(total_hosp_revenue, na.rm = TRUE),  # <- corrected here
  benefit = mean(benefit, na.rm = TRUE),
  medicare = mean(total_hospital_medicare_days, na.rm = TRUE),
  medicaid = mean(total_hospital_medicaid_days, na.rm = TRUE),
  totdis = mean(total_hospital_discharges, na.rm = TRUE),
  mediciaredis = mean(total_hospital_medicare_discharg, na.rm = TRUE),
  mediciaddis = mean(total_hospital_medicaid_discharg, na.rm = TRUE),
  herf_index = mean(herf_index, na.rm = TRUE)
)

# View the results
summary_table
# Calculate standard deviations
sd_table <- summarize(
  herfindahl,
  sd_bed = sd(hospital_beds, na.rm = TRUE),
  sd_payer = sd(total_hospital_employees_on_payr, na.rm = TRUE),
  sd_nopayer = sd(total_hospital_non_paid_workers, na.rm = TRUE),
  sd_resident = sd(interns_and_residents, na.rm = TRUE),
  sd_member = sd(system_member, na.rm = TRUE),
  sd_cost = sd(total_hosp_cost, na.rm = TRUE),
  sd_revenue = sd(total_hosp_revenue, na.rm = TRUE),
  sd_benefit = sd(benefit, na.rm = TRUE),
  sd_medicare = sd(total_hospital_medicare_days, na.rm = TRUE),
  sd_medicaid = sd(total_hospital_medicaid_days, na.rm = TRUE),
  sd_totdis = sd(total_hospital_discharges, na.rm = TRUE),
  sd_medicare_dis = sd(total_hospital_medicare_discharg, na.rm = TRUE),
  sd_medicaid_dis = sd(total_hospital_medicaid_discharg, na.rm = TRUE),
  sd_herf_index = sd(herf_index, na.rm = TRUE)
)

# View the results
sd_table
# Count number of non-missing observations for each variable
n_table <- summarize(
  herfindahl,
  n_bed = sum(!is.na(hospital_beds)),
  n_payer = sum(!is.na(total_hospital_employees_on_payr)),
  n_nopayer = sum(!is.na(total_hospital_non_paid_workers)),
  n_resident = sum(!is.na(interns_and_residents)),
  n_member = sum(!is.na(system_member)),
  n_cost = sum(!is.na(total_hosp_cost)),
  n_revenue = sum(!is.na(total_hosp_revenue)),
  n_benefit = sum(!is.na(benefit)),
  n_medicare = sum(!is.na(total_hospital_medicare_days)),
  n_medicaid = sum(!is.na(total_hospital_medicaid_days)),
  n_totdis = sum(!is.na(total_hospital_discharges)),
  n_medicare_dis = sum(!is.na(total_hospital_medicare_discharg)),
  n_medicaid_dis = sum(!is.na(total_hospital_medicaid_discharg)),
  n_herf_index = sum(!is.na(herf_index))
)

# View the results
n_table
# Anova Test 
# Load required packages
library(dplyr)
library(broom)

# Ensure herf_cat is a factor
df$herf_cat <- as.factor(df$herf_cat)

# List of all continuous variables from your dataset
continuous_vars <- c(
  "hospital_beds",
  "total_hospital_employees_on_payr",
  "total_hospital_non_paid_workers",
  "interns_and_residents",
  "total_hosp_cost",
  "total_hosp_revenue",
  "benefit",
  "total_hospital_medicare_days",
  "total_hospital_medicaid_days",
  "total_hospital_discharges",
  "total_hospital_medicare_discharg",
  "total_hospital_medicaid_discharg",
  "herf_index"
)

# Run ANOVA for each variable
anova_results <- lapply(continuous_vars, function(var) {
  formula <- as.formula(paste(var, "~ herf_cat"))
  aov_result <- aov(formula, data = df)
  tidy(aov_result)[1, c("term", "statistic", "p.value")] %>%
    mutate(variable = var)
})

# Combine results into one table
anova_table <- bind_rows(anova_results) %>%
  select(variable, statistic, p.value)

# View the table
print(anova_table)
library(ggplot2)

# Reshape data: combine cost and revenue into long format
df_long <- df %>%
  select(herf_cat, total_hosp_cost, total_hosp_revenue) %>%
  pivot_longer(cols = c(total_hosp_cost, total_hosp_revenue),
               names_to = "variable",
               values_to = "value")

# Create the density plot
p <- ggplot(df_long, aes(x = value, color = herf_cat, fill = herf_cat)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Density of Hospital Cost and Revenue by Market Competition",
       x = "Value (Cost or Revenue)",
       y = "Density",
       color = "Market Type",
       fill = "Market Type") +
  theme_minimal()

# Print the plot
print(p)
# Computing rations 
df <- df %>%
mutate(
  medicare_ratio = total_hospital_medicare_discharg / total_hospital_discharges,
  medicaid_ratio = total_hospital_medicaid_discharg / total_hospital_discharges
)
# Confirm first few values
head(df %>% select(herf_cat, medicare_ratio, medicaid_ratio))
#Summary stat
summary_stats <- df %>%
  group_by(herf_cat) %>%
  summarise(
    N = n(),
    mean_medicare_ratio = mean(medicare_ratio, na.rm = TRUE),
    sd_medicare_ratio = sd(medicare_ratio, na.rm = TRUE),
    mean_medicaid_ratio = mean(medicaid_ratio, na.rm = TRUE),
    sd_medicaid_ratio = sd(medicaid_ratio, na.rm = TRUE)
  )

print(summary_stats)
# Split dataset by competition level
high <- df %>% filter(herf_cat == 0)   
moderate <- df %>% filter(herf_cat == 1)
low <- df %>% filter(herf_cat == 2) 
# T-test
t_medicare_high_mod <- t.test(high$medicare_ratio, moderate$medicare_ratio)
t_medicare_high_low <- t.test(high$medicare_ratio, low$medicare_ratio)

t_medicaid_high_mod <- t.test(high$medicaid_ratio, moderate$medicaid_ratio)
t_medicaid_high_low <- t.test(high$medicaid_ratio, low$medicaid_ratio)

# Print results
t_medicare_high_mod
t_medicare_high_low
t_medicaid_high_mod
t_medicaid_high_low
# combining into a table 
anova_results <- data.frame(
  Comparison = c("High vs Moderate", "High vs Low", "High vs Moderate", "High vs Low"),
  Variable = c("Medicare Ratio", "Medicare Ratio", "Medicaid Ratio", "Medicaid Ratio"),
  p_value = c(
    t_medicare_high_mod$p.value,
    t_medicare_high_low$p.value,
    t_medicaid_high_mod$p.value,
    t_medicaid_high_low$p.value
  )
)

print(anova_results)
# Medicare Ratio Boxplot
p1 <- ggplot(df, aes(x = herf_cat, y = medicare_ratio, fill = herf_cat)) +
  geom_boxplot() +
  labs(title = "Medicare Discharge Ratio by Market Competition",
       x = "Market Type (0=High, 1=Moderate, 2=Low)",
       y = "Medicare Ratio") +
  theme_minimal()

# Medicaid Ratio Boxplot
p2 <- ggplot(df, aes(x = herf_cat, y = medicaid_ratio, fill = herf_cat)) +
  geom_boxplot() +
  labs(title = "Medicaid Discharge Ratio by Market Competition",
       x = "Market Type (0=High, 1=Moderate, 2=Low)",
       y = "Medicaid Ratio") +
  theme_minimal()

# Print both
print(p1)
print(p2)
