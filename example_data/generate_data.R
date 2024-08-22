
# Generate some synthetic data for testing, etc



# 
library(simsurv)
library(tidyverse)

set.seed(9911)

sample_size <- 500
max_fu_time <- 10
min_fu_time <- 1

{
df <- data.frame(id = 1:sample_size,
                 trt = stats::rbinom(sample_size, 1L, 0.5),
                 age = stats::rnorm(sample_size,70,10)
)

# If we anticipate that there's dependent censoring due to e.g.
# population drift, handle it by sorting df prior to finalising
# recruitment time

df %>%
  mutate(
    recruitment_time= cumsum(rexp(sample_size,rate = 50)),
    fu_time = max_fu_time-recruitment_time,
    fu_time = ifelse(fu_time<1,min_fu_time,fu_time)
  ) -> df



######################

# Generate outcomes 

######################


# Some ordinal outcome, assumed to be proportional odds
df$rankin <-  2 + -0.05*df$age + 0.5*df$trt

# Add intercepts and sample
df$rankin <- apply(
  outer(df$rankin,c(-2, -1, -0.5, 0,0.5,1),"+"),
  1,
  function(x){
    x <- exp(x)/(1+exp(x))
    x <- c(0,x,1)
    x <- diff(x)
    
    sample((1:length(x))-1,size = 1, prob=x)
  }
)

######################

# Generate Survival outcomes

######################

# Get death times
df <- left_join(df,
                simsurv(lambdas = 0.001, gammas = 1.5,
                        betas = c(trt = -1, age=0.08),
                        x = df, maxt = 400) %>%
                  rename(deathTime=eventtime, deathStatus = status)
                )


# Certain fraction of people will be hospitalised.
# Get individual probability of this, assuming logistic regression probability
df$hospitalised_tmp <- -5 + 0.05*df$age + df$trt
df$hospitalised_tmp <- (exp(df$hospitalised)/(1+exp(df$hospitalised)) )
df$hospitalised_tmp <- sapply(df$hospitalised,function(x){stats::rbinom(1L, 1L, x)})

df <- left_join(df,
                simsurv(lambdas = 0.002, gammas = 2,
                        betas = c(trt = -0.05, age=0.12),
                        x = df, maxt = 400) %>%
                  rename(hospTime=eventtime, hospStatus = status)
)

# Set hospital time as infinity if we weren't hospitalised
df[df$hospitalised_tmp==0,"hospTime"] <- Inf
  

# Set up censoring

df %>%
  mutate(
    
    # Competing risk
    hospStatus = ifelse(hospTime < deathTime,hospStatus,2),
    hospTime  = ifelse(hospStatus == 2, deathTime , hospTime),
    
    # End of followup
    hospStatus = ifelse(hospTime < fu_time,hospStatus,0),
    hospTime  = ifelse(hospStatus == 0, fu_time, hospTime),
    
    deathStatus = ifelse(deathTime < fu_time,deathStatus,0),
    deathTime  = ifelse(deathStatus == 0, fu_time, deathTime)
    
  ) %>%
  select(-hospitalised_tmp) -> df

}






