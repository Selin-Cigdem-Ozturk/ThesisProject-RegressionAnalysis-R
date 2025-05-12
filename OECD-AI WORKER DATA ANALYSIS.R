# === 1. LOAD REQUIRED LIBRARIES ===
library(haven)
library(dplyr)
library(ggplot2)
library(car)
library(mice)
library(lmtest)
library(stargazer)
library(forcats)
library(psych)
library(magrittr)

# === 2. LOAD AND PREPARE DATA ===
worker_data <- read_dta("/Users/selincigdemozturk/Desktop/OECD data/worker_shared.dta")

# Convert decision-making variables to numeric
q30_items <- c("impactdecisionsuser_1", "impactdecisionsuser_2", "impactdecisionsuser_3")
worker_data[q30_items] <- lapply(worker_data[q30_items], as.numeric)

# Handle Worry Index 8s as NA
worker_data <- worker_data %>%
  mutate(across(c(attitudesuser_1, attitudesuser_2, attitudesuser_3), ~ na_if(as.numeric(.), 8)))

# Create all perception indices
worker_data <- worker_data %>%
  mutate(
    AIDI = rowMeans(across(all_of(q30_items)), na.rm = TRUE),
    Autonomy_Index = rowMeans(across(c("impactautonomyuser_1", "impactautonomyuser_2")), na.rm = TRUE),
    Trust_Index = rowMeans(across(c("trustcompany_1", "trustcompany_2", "trustcompany_3", "trustcompany_4", "trustcompany_5")), na.rm = TRUE),
    Worry_Index = rowMeans(across(c("attitudesuser_1", "attitudesuser_2", "attitudesuser_3")), na.rm = TRUE),
    Fairness = impactmanagementuser
  )

# === 3. RECODE CATEGORICAL VARIABLES AS FACTORS ===
worker_data <- worker_data %>%
  mutate(
    employeeagecat2 = factor(employeeagecat2, levels = c(1:6, 9),
                             labels = c("Age <16", "16–24", "25–34", "35–49", "50–64", "65+", "Unspecified")),
    sector = factor(sector, levels = 1:2, labels = c("Finance", "Manufacturing")),
    employeesex = factor(employeesex, levels = 1:3, labels = c("Male", "Female", "Unspecified")),
    education = factor(education, levels = c(1, 2, 9), labels = c("Yes", "No", "No Answer")),
    businesssizebands = factor(businesssizebands, levels = 1:6,
                               labels = c("1–19", "20–49", "50–99", "100–249", "250–499", "500+")),
    employeerole = factor(employeerole, levels = 1:10,
                          labels = c("Manager", "Professional", "Technician", "Clerical", "Sales",
                                     "Craft", "Operator", "Elementary", "Other", "No Answer"))
  )

# === 4. BUILD REGRESSION MODELS ===
regression_vars <- c("sector", "employeesex", "employeeagecat2", "education", "businesssizebands", "employeerole")

models <- list(
  AIDI     = lm(as.formula(paste("AIDI ~", paste(regression_vars, collapse = " + "))), data = worker_data),
  Autonomy = lm(as.formula(paste("Autonomy_Index ~", paste(regression_vars, collapse = " + "))), data = worker_data),
  Trust    = lm(as.formula(paste("Trust_Index ~", paste(regression_vars, collapse = " + "))), data = worker_data),
  Fairness = lm(as.formula(paste("Fairness ~", paste(regression_vars, collapse = " + "))), data = worker_data),
  Worry    = lm(as.formula(paste("Worry_Index ~", paste(regression_vars, collapse = " + "))), data = worker_data)
)

# === 5. CHECK RELIABILITY (CRONBACH'S ALPHA) ===
alpha(worker_data[q30_items])
alpha(worker_data[c("impactautonomyuser_1", "impactautonomyuser_2")])
alpha(worker_data[c("trustcompany_1", "trustcompany_2", "trustcompany_3", "trustcompany_4", "trustcompany_5")])
alpha(worker_data[c("attitudesuser_1", "attitudesuser_2", "attitudesuser_3")])

# === 6. MULTICOLLINEARITY CHECK ===
lapply(models, vif)

# === 7. VISUALIZATION FUNCTION ===
# Define the function
plot_scatter_lm <- function(data, x, y, title) {
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "red") +
    labs(title = title) +
    theme_minimal()
  
  print(p)
}

# Create some dummy data
mydata <- data.frame(
  var1 = rnorm(100),
  var2 = rnorm(100)
)

# Call the function
plot_scatter_lm(mydata, "var1", "var2", "Scatter Plot with Linear Fit")



# === 8. GENERATE PLOTS ===
plot_scatter_lm(worker_data, "Fairness", "AIDI", "AIDI vs Fairness")
plot_scatter_lm(worker_data, "Autonomy_Index", "AIDI", "AIDI vs Autonomy")
plot_scatter_lm(worker_data, "Trust_Index", "AIDI", "AIDI vs Trust")
plot_scatter_lm(worker_data, "Worry_Index", "AIDI", "AIDI vs Worry")

# === 9. RESIDUAL DIAGNOSTICS (AIDI Model) ===
par(mfrow = c(2, 2))
hist(residuals(models$AIDI), main = "Histogram of Residuals", col = "blue")
qqnorm(residuals(models $ AIDI)); qqline(residuals(models$AIDI), col = "red")
plot(models $ AIDI$fitted.values, residuals(models$AIDI), main = "Residuals vs Fitted")
abline(h = 0, col = "red")
bptest(models$AIDI)

# === 10. INFLUENTIAL OBSERVATIONS ===
cooks <- cooks.distance(models$AIDI)
threshold <- 4 / nrow(worker_data)
influential <- which(cooks > threshold)
plot(cooks, type = "h", main = "Cook’s Distance"); abline(h = threshold, col = "red")
worker_data_clean <- worker_data[-influential, ]

# === 11. RE-RUN MODEL WITHOUT OUTLIERS ===
model_clean <- lm(AIDI ~ Fairness + Worry_Index + Autonomy_Index + Trust_Index +
                    sector + employeesex + employeeagecat2 + education +
                    businesssizebands + employeerole, data = worker_data_clean)
summary(model_clean)

# === 12. EXPORT FINAL MODELS USING STARGAZER ===

# Display Summary of Each Model
lapply(models, summary)

# Check for Missing Data in Dependent Variables
sapply(c("AIDI", "Autonomy_Index", "Trust_Index", "Worry_Index", "Fairness"), function(var) {
  sum(is.na(worker_data[[var]]))
})

# Identify Valid Model Names
valid_model_names <- names(models)[sapply(models, function(m) inherits(m, "lm") && all(!is.na(coef(m))))]

# Subset Valid Models
valid_models <- models[valid_model_names]

# Labels for Stargazer
labels_map <- c(
  AIDI = "Decision Making",
  Autonomy = "Autonomy",
  Trust = "Trust",
  Worry = "Worry",
  Fairness = "Fairness"
)
dep_labels <- labels_map[valid_model_names]



# Export Final Regression Table
stargazer(valid_models,
          type = "text",
          dep.var.labels = dep_labels,
          omit.stat = c("f", "ser"),
          covariate.labels = c(
            "Manufacturing", "Female", "Gender not specified", 
            "Age 25-34", "Age 35-49", 
            "Age 50-64", "Age 65+", 
            "No university degree", "University degree or above",
            "Business size 20-49", "Business size 50-99",
            "Business size 100-249", "Business size 250-499",
            "Business size 500+", "Business size not mentioned",
            "Professional Employee", "Technician and Associate",
            "Clerical Support", "Service and Sales", 
            "Craft and Related Trades", "Plant and Machine Operator",
            "Elementary Occupation", "Other Roles", "Occupation not mentioned"
          ))

