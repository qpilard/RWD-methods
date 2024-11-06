# Ensure reproducibility
set.seed(42)  

# Number of patients
N <- 600

# Patient IDs
ID <- 1:N

# Generation of base variables
AGE <- pmax(pmin(round(rnorm(N, mean = 32, sd = 6.19)), 80), 18)
GENDER <- sample(0:1, N, replace = TRUE)
SOCIO_ECO <- sample(1:5, N, replace = TRUE)
SOCIO_ECO_2 <- as.numeric(SOCIO_ECO == 2)
SOCIO_ECO_3 <- as.numeric(SOCIO_ECO == 3)
SOCIO_ECO_4 <- as.numeric(SOCIO_ECO == 4)
SOCIO_ECO_5 <- as.numeric(SOCIO_ECO == 5)
BECK <- pmax(pmin(round(rnorm(N, mean = 30, sd = 9.33)), 63), 0)

# Utility function to generate binary variables based on a logistic model
logit_prob <- function(eta) {
  1 / (1 + exp(-eta))
}

# Coefficients for predictors
coefficients <- list(
  intercept = -1.5,      
  age = -0.04,          
  gender = -0.3,        
  beck = 0.1,         
  socio_eco_2 = -0.10,
  socio_eco_3 = -0.12,
  socio_eco_4 = -0.18,
  socio_eco_5 = -0.25
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

TREAT <- rbinom(N, 1, logit_prob(eta_treat + rnorm(N)))

# Generation of time to treat (scenario 2 only)
TIME_TO_TREAT <- numeric(N)
for (i in 1:N) {
  TIME_TO_TREAT[i] <- ifelse(TREAT[i] == 1,
                             pmax(pmin(round(rnorm(N, mean = 10, sd = 3)), 30), 0),
                             pmax(pmin(round(rnorm(N, mean = 14, sd = 5)), 30), 0))
}

# Generation of relapse (EVENT) and time until relapse (TIME_TO_EVENT)
base_hazard <- 0.005
EVENT <- numeric(N)
TIME_TO_EVENT <- numeric(N)
max_follow_up <- 52  # Maximum follow-up duration in weeks
coefficients <- list(
  intercept = -4,                 
  age = -0.01, #Younger individuals are more likely to experience PDD                      
  gender = 0.1, #Women are more likely to experience PDD                   
  socio_eco_2 = -0.05, #Lower SES level is more likely to experience PDD             
  socio_eco_3 = -0.10,             
  socio_eco_4 = -0.15,            
  socio_eco_5 = -0.20,            
  beck = 0.2, #Higher Beck score is more likely to experience PDD                    
  treat = -0.5 #Protective effect of Duloxyn                
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
  
  adjusted_hazard <- base_hazard * exp(eta_event)
  
  # Generate time to event
  TIME_TO_EVENT[i] <- round(rexp(1, rate = adjusted_hazard), 0) + TIME_TO_TREAT[i]
  
  # Ensure EVENT only occurs within 52 weeks
  EVENT[i] <- as.integer(TIME_TO_EVENT[i] < max_follow_up)
  
  # Set TIME_TO_EVENT to max_follow_up if EVENT does not occur
  if (EVENT[i] == 0 && runif(1) < 0.10) {
    TIME_TO_EVENT[i] <- sample(1:52, 1)  # Assign random time between 1 and 52 weeks
    EVENT[i] <- 0  # Confirm no event despite dropout time assignment
  }
  
  # Ensure TIME_TO_EVENT doesn't exceed the maximum follow-up period (52 weeks)
  TIME_TO_EVENT[i]<-min(TIME_TO_EVENT[i], max_follow_up)
}

# Organize data in a data frame
data <- data.frame(ID, AGE, GENDER, SOCIO_ECO, BECK, TREAT, EVENT, TIME_TO_EVENT, TIME_TO_TREAT)

data$GENDER <- factor(data$GENDER, levels = c(0, 1), labels = c("Male", "Female"))
data$SOCIO_ECO <- factor(data$SOCIO_ECO, levels = c(1, 2, 3, 4, 5), labels = c("Very low", "Low", "Moderate", "High", "Very high"))
data$TREAT <- factor(data$TREAT, levels = c(0, 1), labels = c("Sertralex","Duloxyn"))