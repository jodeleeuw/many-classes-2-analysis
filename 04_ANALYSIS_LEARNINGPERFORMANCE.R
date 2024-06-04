## ----------------------------------
## ANALYSIS.LEARNINGPERFORMANCE.R
## ----------------------------------
# Gets preprocessed data from OSF
# and carries out analyses on 
# participant's (posttest) learning 
# outcomes.

# Clean-up
rm(list=ls())

# Required Libraries
require(tidyverse)
require(tidybayes)

# Source DBDA2E Helpers from OSF
source("https://osf.io/4upzk/download")

# Load the data
# load(file = "analysis_set.Rdata")
load(url("https://osf.io/yks7a/download"))

# LEARNING OUTCOMES

# Simplify datFrm for only the participants, conditions, exposure, and outcome
# ...in long format
datFrm1 <- datFrm %>% select(participant_id,course_id,exposure_order,outcome_prequestions,outcome_control)
datFrm1 <- datFrm1 %>% pivot_longer(!c(participant_id,course_id,exposure_order),names_to = "condition_name", values_to = "outcome_score")
datFrm1 <- datFrm1 %>% mutate(condition_name = if_else(condition_name == "outcome_control","control","prequestions"))
datFrm1 <- datFrm1 %>% mutate(exposure = case_when(exposure_order=="control_then_prequestion" & condition_name == "control" ~ 0,
                                                   exposure_order=="control_then_prequestion" & condition_name == "prequestions" ~ 1,
                                                   exposure_order=="prequestions_then_control" & condition_name == "control" ~ 1,
                                                   exposure_order=="prequestions_then_control" & condition_name == "prequestions" ~ 0,
                                                   .default = NA)) %>% select(!exposure_order)
datFrm2 <- datFrm %>% select(participant_id,course_id,points_possible_prequestions,points_possible_control)
datFrm2 <- datFrm2 %>% pivot_longer(!c(participant_id,course_id),names_to = "condition_name", names_prefix = "points_possible_", values_to = "points_possible")
datFrm <- merge(datFrm1,datFrm2,by=c("participant_id","course_id","condition_name"),all.x=TRUE)
rm(datFrm1,datFrm2)

# Additional formatting adjustments
datFrm <- na.omit(datFrm) # exclusion of missing outcome scores is preregistered
datFrm$participant_id_factor <- as.numeric(as.factor(datFrm$participant_id))
datFrm$course_id_factor <-as.numeric(as.factor(datFrm$course_id))

# Prepare data for JAGS
sub <- datFrm$participant_id_factor
x <- if_else(datFrm$condition_name == "prequestions",1,0) # "Pre-Questions"=1 & "No PreQuestion"=0
w <- datFrm$exposure # exposure1=0 & exposure2=1
c <- rep(0,length(unique(datFrm$participant_id_factor))) # initialize a placeholder for courses
for (s in 1:length(unique(datFrm$participant_id_factor))) {
  c[s] <- unique(datFrm$course_id_factor[datFrm$participant_id_factor==s]) # populate courses, ordered by participant_id_factor
}
y <- datFrm$outcome_score
n <- datFrm$points_possible
nRowsData <- nrow(datFrm)
nSubs <- length(unique(sub))
nClasses <- length(unique(c))

# Package for JAGS
dataList = list(
  y = y ,
  n = n ,
  x = x ,
  w = w ,
  c = c ,
  sub = sub ,
  nRowsData = nRowsData ,
  nSubs = nSubs ,
  nClasses = nClasses
)

if (TRUE) { # If we're going to re-run the model...
  
  modelString = "
  model {
    
    for (i in 1:nRowsData) {
      y[i] ~ dbin( ilogit(p[i]), n[i])
      p[i] <- intercept[sub[i]] 
              + (beta_x[sub[i]] * x[i]) # x is 0 and 1
              + (beta_w[sub[i]] * w[i]) # w is 0 and 1
    }
    
    # Subject-level
    for (s in 1:nSubs) {
      intercept[s] ~ dnorm(mu_int_c[c[s]], 1/sigma_int_c[c[s]]^2) 
      beta_x[s] ~ dnorm(mu_betax_c[c[s]], 1/sigma_betax_c[c[s]]^2)
      beta_w[s] ~ dnorm(mu_betaw_c[c[s]], 1/sigma_betaw_c[c[s]]^2)
    }
    
    # Class-level 
    for (g in 1:nClasses) {
      mu_int_c[g]      ~ dnorm(0.0,1/2^2)
      sigma_int_c[g]   ~ dgamma(1.64,0.32)  # mode=2, sd=4
      mu_betax_c[g]    ~ dnorm(mu_betax,1/sigma_betax^2) 
      sigma_betax_c[g] ~ dgamma(1.64,0.32)  # mode=2, sd=4
      mu_betaw_c[g]    ~ dnorm(mu_betaw,1/sigma_betaw^2) 
      sigma_betaw_c[g] ~ dgamma(1.64,0.32)  # mode=2, sd=4
      
      # Store estimated probabilities for different x values
      x0_p_c[g] <- ilogit(mu_int_c[g]+(mu_betaw_c[g]/2))
      x1_p_c[g] <- ilogit(mu_int_c[g]+mu_betax_c[g]+(mu_betaw_c[g]/2))
      
      # Store estimated probabilities for different w values
      w0_p_c[g] <- ilogit(mu_int_c[g]+(mu_betax_c[g]/2))
      w1_p_c[g] <- ilogit(mu_int_c[g]+mu_betaw_c[g]+(mu_betax_c[g]/2))
      
    }
    
    # Condition-level 
    mu_betax    ~ dnorm(0.0,1/2^2) 
    sigma_betax ~ dgamma(1.64,0.32)  # mode=2, sd=4
    mu_betaw    ~ dnorm(0.0,1/2^2) 
    sigma_betaw ~ dgamma(1.64,0.32)  # mode=2, sd=4
    
  } " # close quote for modelString
  
  # Write out modelString to a text file
  writeLines( modelString , con='TEMPmodel.txt')
  adaptSteps = 500
  burnInSteps = 1000
  numSavedSteps = 10000 # This will be changed in final analysis to achieve a minimum ESS of 20000
  thinSteps = 1
  nChains = 4
  parameters = c( "mu_int_c" , "sigma_int_c" , "mu_betax_c" , "sigma_betax_c" , "mu_betax" , "sigma_betax" , "sigma_betaw" ,
                  "mu_betaw_c" , "sigma_betaw_c" , "mu_betaw" , "x0_p_c" , "x1_p_c" , "w0_p_c" , "w1_p_c") 
  runJagsOut <- run.jags( method="parallel" ,
                          model="TEMPmodel.txt" , 
                          monitor=parameters , 
                          data=dataList ,
                          n.chains=nChains ,
                          adapt=adaptSteps ,
                          burnin=burnInSteps , 
                          sample=ceiling(numSavedSteps/nChains) ,
                          thin=thinSteps ,
                          summarise=FALSE ,
                          plots=FALSE )
  CodaSamples = as.mcmc.list( runJagsOut )
  save( CodaSamples , file="CodaSamples_CombinedPerformance.Rdata")
  
  # Diagnose
  for ( parName in c("mu_betax","mu_betaw") ) {
    diagMCMC( codaObject=CodaSamples , parName=parName)
  }
  
} else {
  
  load( file="CodaSamples_CombinedPerformance.Rdata")
  
}

# Switch into matrix form for analysis
mcmcMat = as.matrix(CodaSamples,chains=TRUE)

# # Plot the posterior distribution of the effect of X
postInfo = plotPost( mcmcMat[, "mu_betax" ] )

# Model summary for each class
model_summary <- datFrm %>% group_by(course_id,course_id_factor) %>% 
  summarize(n = length(unique(participant_id))) %>% mutate(course_id = as.character(course_id), value = 0, lower = 0, upper = 0, ess = 0, variable = 0)
for (ci in 1:length(unique(c))) {
  varname0 <- paste("x0_p_c[",ci,"]",sep="")
  varname1 <- paste("x1_p_c[",ci,"]",sep="")
  out <- summarizePost( mcmcMat[, varname1] - mcmcMat[, varname0] )
  model_summary$value[model_summary$course_id_factor==ci] <- out["Mode"]
  model_summary$lower[model_summary$course_id_factor==ci] <- out["HDIlow"]
  model_summary$upper[model_summary$course_id_factor==ci] <- out["HDIhigh"]
  model_summary$ess[model_summary$course_id_factor==ci]   <- out["ESS"]
}
# Overall effect (do it by hand)
out <- summarizePost( (   ( mcmcMat[, "x1_p_c[1]" ] - mcmcMat[, "x0_p_c[1]" ] ) +
                          ( mcmcMat[, "x1_p_c[2]" ] - mcmcMat[, "x0_p_c[2]" ] ) +
                          ( mcmcMat[, "x1_p_c[3]" ] - mcmcMat[, "x0_p_c[3]" ] ) +
                          ( mcmcMat[, "x1_p_c[4]" ] - mcmcMat[, "x0_p_c[4]" ] ) +
                          ( mcmcMat[, "x1_p_c[5]" ] - mcmcMat[, "x0_p_c[5]" ] ) +
                          ( mcmcMat[, "x1_p_c[6]" ] - mcmcMat[, "x0_p_c[6]" ] ) +
                          ( mcmcMat[, "x1_p_c[7]" ] - mcmcMat[, "x0_p_c[7]" ] ) +
                          ( mcmcMat[, "x1_p_c[8]" ] - mcmcMat[, "x0_p_c[8]" ] ) +
                          ( mcmcMat[, "x1_p_c[9]" ] - mcmcMat[, "x0_p_c[9]" ] ) +
                          ( mcmcMat[, "x1_p_c[10]" ] - mcmcMat[, "x0_p_c[10]" ] ) +
                          ( mcmcMat[, "x1_p_c[11]" ] - mcmcMat[, "x0_p_c[11]" ] ) +
                          ( mcmcMat[, "x1_p_c[12]" ] - mcmcMat[, "x0_p_c[12]" ] ) +
                          ( mcmcMat[, "x1_p_c[13]" ] - mcmcMat[, "x0_p_c[13]" ] ) +
                          ( mcmcMat[, "x1_p_c[14]" ] - mcmcMat[, "x0_p_c[14]" ] ) +
                          ( mcmcMat[, "x1_p_c[15]" ] - mcmcMat[, "x0_p_c[15]" ] ) +
                          ( mcmcMat[, "x1_p_c[16]" ] - mcmcMat[, "x0_p_c[16]" ] ) +
                          ( mcmcMat[, "x1_p_c[17]" ] - mcmcMat[, "x0_p_c[17]" ] ) +
                          ( mcmcMat[, "x1_p_c[18]" ] - mcmcMat[, "x0_p_c[18]" ] ) +
                          ( mcmcMat[, "x1_p_c[19]" ] - mcmcMat[, "x0_p_c[19]" ] ) +
                          ( mcmcMat[, "x1_p_c[20]" ] - mcmcMat[, "x0_p_c[20]" ] ) +
                          ( mcmcMat[, "x1_p_c[21]" ] - mcmcMat[, "x0_p_c[21]" ] ) +
                          ( mcmcMat[, "x1_p_c[22]" ] - mcmcMat[, "x0_p_c[22]" ] ) +
                          ( mcmcMat[, "x1_p_c[23]" ] - mcmcMat[, "x0_p_c[23]" ] ) +
                          ( mcmcMat[, "x1_p_c[24]" ] - mcmcMat[, "x0_p_c[24]" ] ) +
                          ( mcmcMat[, "x1_p_c[25]" ] - mcmcMat[, "x0_p_c[25]" ] ) +
                          ( mcmcMat[, "x1_p_c[26]" ] - mcmcMat[, "x0_p_c[26]" ] ) +
                          ( mcmcMat[, "x1_p_c[27]" ] - mcmcMat[, "x0_p_c[27]" ] ) +
                          ( mcmcMat[, "x1_p_c[28]" ] - mcmcMat[, "x0_p_c[28]" ] ) +
                          ( mcmcMat[, "x1_p_c[29]" ] - mcmcMat[, "x0_p_c[29]" ] ) +
                          ( mcmcMat[, "x1_p_c[30]" ] - mcmcMat[, "x0_p_c[30]" ] ) ) / 30)
average_row <- data.frame(course_id="Average Effect",course_id_factor=0,n=0,value=out["Mode"],lower=out["HDIlow"],upper=out["HDIhigh"],ess=0, variable = 1, y="Average Effect")
model_summary <- rbind(model_summary,average_row)
# Overall effect (do it by hand)
out <- summarizePost( mcmcMat[, "mu_betax" ] )
global_row <- data.frame(course_id="Global Effect",course_id_factor=32,n=0,value=out["Mode"],lower=out["HDIlow"],upper=out["HDIhigh"],ess=0, variable = 1, y="Global Effect")
model_summary <- rbind(model_summary,global_row)
model_summary$y = as.factor(model_summary$course_id)
model_summary$y <- fct_reorder(model_summary$course_id,model_summary$value)
model_summary$variable <- as.factor(model_summary$variable)

ggplot(data = model_summary, aes(x=value,y=y,xmin=lower,xmax=upper)) +
  geom_vline(xintercept=0, linetype='dashed', color="grey50") +
  geom_pointinterval(show_point=F) +
  geom_point(size=2)+
  ylab("Class ID") + xlab("Relative Benefit of Prequestions for Learning") +
  facet_grid(rows=vars(variable), scales = 'free', space='free') +
  theme_minimal() +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank(),
    axis.title.x = element_text(size = 11)
  )



