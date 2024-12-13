# Ensure reproducibility
set.seed(42)  

# Number of patients
N <- 800

# Patient IDs
ID <- 1:N

# Generation of base variables
AGE <- pmax(pmin(round(rnorm(N, mean = 32, sd = 6.19)), 80), 18)  # Age bounded between 18 and 80
GENDER <- sample(0:1, N, replace = TRUE)  # Binary gender variable
SOCIO_ECO <- sample(1:5, N, replace = TRUE)  # Socioeconomic status categories
SOCIO_ECO_2 <- as.numeric(SOCIO_ECO == 2)  # Dummy variable for SOCIO_ECO == 2
SOCIO_ECO_3 <- as.numeric(SOCIO_ECO == 3)  # Dummy variable for SOCIO_ECO == 3
SOCIO_ECO_4 <- as.numeric(SOCIO_ECO == 4)  # Dummy variable for SOCIO_ECO == 4
SOCIO_ECO_5 <- as.numeric(SOCIO_ECO == 5)  # Dummy variable for SOCIO_ECO == 5
BECK <- pmax(pmin(round(rnorm(N, mean = 30, sd = 9.33)), 63), 0)  # Beck score bounded between 0 and 63

# Utility function to generate binary variables based on a logistic model
logit_prob <- function(eta) {
  1 / (1 + exp(-eta))  # Logistic function
}

# Coefficients for predictors
coefficients <- list(
  intercept = -1.5,      # Base intercept for treatment assignment
  age = -0.04,           # Negative effect of age
  gender = -0.3,         # Negative effect of gender (e.g., Female is less likely to be assigned)
  beck = 0.1,            # Positive effect of Beck score
  socio_eco_2 = -0.10,   # Negative effect for SOCIO_ECO_2
  socio_eco_3 = -0.12,   # Negative effect for SOCIO_ECO_3
  socio_eco_4 = -0.18,   # Negative effect for SOCIO_ECO_4
  socio_eco_5 = -0.25    # Stronger negative effect for SOCIO_ECO_5
)

# Generation of initial treatment (TREAT)
eta_treat <- coefficients$intercept + 
  coefficients$age * AGE + 
  coefficients$gender * GENDER +
  coefficients$beck * BECK + 
  coefficients$socio_eco_2 * SOCIO_ECO_2 +
  coefficients$socio_eco_3 * SOCIO_ECO_3 + 
  coefficients$socio_eco_4 * SOCIO_ECO_4 +
  coefficients$socio_eco_5 * SOCIO_ECO_5

TREAT <- rbinom(N, 1, logit_prob(eta_treat + rnorm(N)))  # Simulating treatment assignment

# Coefficients for treatment delay based on treatment status
coefficients_delay <- list(
  sertralex = list(
    intercept = 10,      # Base parameter for delay
    age = -0.02,         # Negative effect of age on delay
    gender = 0.1,        # Positive effect of gender on delay
    beck = -0.05,        # Negative effect of Beck score on delay
    socio_eco_2 = 0.2,
    socio_eco_3 = 0.15,
    socio_eco_4 = 0.1,
    socio_eco_5 = 0.05
  ),
  duloxyn = list(
    intercept = 15,
    age = -0.02,
    gender = 0.1,
    beck = -0.05,
    socio_eco_2 = 0.2,
    socio_eco_3 = 0.15,
    socio_eco_4 = 0.1,
    socio_eco_5 = 0.05
  )
)

# Initialization of TIME_TO_TREAT variable
TIME_TO_TREAT <- numeric(N)

# Generation of treatment initiation delays
for (i in 1:N) {
  # Select appropriate coefficients
  delay_coeffs <- if (TREAT[i] == 1) {
    coefficients_delay$duloxyn
  } else {
    coefficients_delay$sertralex 
  }
  
  # Compute the mean
  linear_pred <- delay_coeffs$intercept +
    delay_coeffs$age * AGE[i] +
    delay_coeffs$gender * GENDER[i] +
    delay_coeffs$beck * BECK[i] +
    delay_coeffs$socio_eco_2 * SOCIO_ECO_2[i] +
    delay_coeffs$socio_eco_3 * SOCIO_ECO_3[i] +
    delay_coeffs$socio_eco_4 * SOCIO_ECO_4[i] +
    delay_coeffs$socio_eco_5 * SOCIO_ECO_5[i]
  
  # Generate time from a normal distribution
  TIME_TO_TREAT[i] <- round(rnorm(n = 1, mean = linear_pred + rnorm(n = 1), sd = 2.5), 0)
}

# Generation of relapse (EVENT) and time until relapse (TIME_TO_EVENT) in days
base_hazard <- 0.005  # Baseline hazard rate
EVENT <- numeric(N)
TIME_TO_EVENT <- numeric(N)
max_follow_up <- 365  # Maximum follow-up duration in days
coefficients <- list(
  intercept = -4,       # Base intercept for event hazard
  age = -0.01,          # Younger individuals are more likely to experience PDD                      
  gender = 0.1,         # Women are more likely to experience PDD                   
  socio_eco_2 = -0.05,  # Lower SES is more likely to experience PDD             
  socio_eco_3 = -0.10,             
  socio_eco_4 = -0.15,            
  socio_eco_5 = -0.20,            
  beck = 0.2,           # Higher Beck score increases the likelihood of PDD                    
  treat = -0.2          # Protective effect of Duloxyn                
)

for (i in 1:N) {
  eta_event <- coefficients$intercept + 
    coefficients$age * AGE[i] + 
    coefficients$gender * GENDER[i] +
    coefficients$beck * BECK[i] + 
    coefficients$socio_eco_2 * SOCIO_ECO_2[i] +
    coefficients$socio_eco_3 * SOCIO_ECO_3[i] + 
    coefficients$socio_eco_4 * SOCIO_ECO_4[i] +
    coefficients$socio_eco_5 * SOCIO_ECO_5[i] + 
    coefficients$treat * TREAT[i]
  
  adjusted_hazard <- base_hazard * exp(eta_event)  # Adjust hazard rate
  
  # Generate time to event in days
  TIME_TO_EVENT[i] <- round(rexp(1, rate = adjusted_hazard), 0)
  
  # Ensure EVENT only occurs within 365 days
  if (TIME_TO_EVENT[i] < max_follow_up) {
    EVENT[i] <- 1
  } else {
    EVENT[i] <- 0
  }
  
  # Ensure TIME_TO_EVENT doesn't exceed the maximum follow-up period
  TIME_TO_EVENT[i] <- min(TIME_TO_EVENT[i], max_follow_up)
  
}

# Organize data in a data frame
data <- data.frame(ID, AGE, GENDER, SOCIO_ECO, BECK, TREAT, TIME_TO_EVENT, EVENT, TIME_TO_TREAT)

# Filter data based on conditions
data <- data %>%
  filter(TIME_TO_EVENT > TIME_TO_TREAT & TIME_TO_TREAT < 30 & TIME_TO_EVENT > 0)

# Recategorize variables
data$GENDER <- factor(data$GENDER, levels = c(0, 1), labels = c("Male", "Female"))
data$SOCIO_ECO <- factor(data$SOCIO_ECO, levels = c(1, 2, 3, 4, 5), labels = c("Very low", "Low", "Moderate", "High", "Very high"))
data$TREAT <- factor(data$TREAT, levels = c(0, 1), labels = c("Sertralex", "Duloxyn"))
