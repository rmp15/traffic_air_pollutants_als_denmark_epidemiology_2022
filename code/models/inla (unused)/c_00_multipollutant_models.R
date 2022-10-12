########################################################################
# BAYESIAN (R-INLA) LOGISTIC REGRESSION MODELS
########################################################################

################## DEVELOPING MODELS FOR PM2.5 only (fmls 0 - 14) ##################

# 0. logistic regression of case status ~ Slope
fml0 =   ck ~ 1 # intercept

# 1. logistic regression of case status ~ PM2.5
fml1 =  update(fml0, ~ . +
                 PM25) # slope of PM

# 2. logistic regression of case status ~ PM2.5 + confounders
fml2 =  update(fml1, ~ . +
                 family_SES  + # 6 levels
                 place_birth + # 6 levels
                 civst + # 4 levels
                 residence) # confounders

# 3. logistic regression of case status ~ PM2.5 + confounders + birth year
fml3 =  update(fml2, ~ . +
                 birth_year) # birth year

# 4. logistic regression of case status ~ PM2.5 + confounders + birth year + age
fml4 =  update(fml3, ~ . +
                 age_diag) # age

# 5. logistic regression of case status ~ PM2.5 + confounders + birth year + age + sex
fml5 =  update(fml4, ~ . +
                 sex) # sex

# 6. logistic regression of case status ~ PM2.5 + confounders + birth year + age + sex + byear*age_atdiag
fml6 =  update(fml5, ~ . +
                 f(birth_year2,age_diag,model="iid")) # byear and age_atdiag interaction

# 7. logistic regression of case status ~ PM2.5 + confounders + birth year + age + sex + byear*age_atdiag + byear*sex
fml7 =  update(fml6, ~ . +
                 f(birth_year3,sex,model="iid")) # byear and sex interaction

# 8. logistic regression of case status ~ PM2.5 + confounders + birth year + age + sex + byear*age_atdiag + byear*sex + age*sex
fml8 =  update(fml7, ~ . +
                 f(age_diag2,sex,model="iid")) # age and sex interaction

# 9. logistic regression of case status ~ PM2.5 + confounders + birth year + age + sex + byear*age_atdiag + byear*sex + age*sex + byear*age*sex
fml9 =  fml8
# update(fml8, ~ . +
# byear*age*sex) # birth year and age and sex interaction

################## INTRODUCE OTHER POLLUTANTS (fmls 10 - 19) ##################

# 10. logistic regression of case status ~ PM2.5 + confounders + birth year + age + sex + byear*age_atdiag + byear*sex + age*sex + byear*age*sex + NOX
fml10 =  update(fml9, ~ . +
                  NOX) # slope of NOX

# 11. logistic regression of case status ~ PM2.5 + confounders + birth year + age + sex + byear*age_atdiag + byear*sex + age*sex + byear*age*sex + NOX + CO
fml11 =  update(fml10, ~ . +
                  CO) # slope of CO

# 12. logistic regression of case status ~ PM2.5 + confounders + birth year + age + sex + byear*age_atdiag + byear*sex + age*sex + byear*age*sex + NOX + CO + EC
fml12 =  update(fml11, ~ . +
                  EC) # slope of EC

# 13. logistic regression of case status ~ PM2.5 + confounders + birth year + age + sex + byear*age_atdiag + byear*sex + age*sex + byear*age*sex + NOX + CO + EC + O3
fml13 =  update(fml12, ~ . +
                  O3) # slope of O3

# 14. logistic regression of case status ~ PM2.5 + confounders + birth year + age + sex + byear*age_atdiag + byear*sex + age*sex + byear*age*sex + NOX + CO + EC + O3 + NO2
fml14 =  update(fml13, ~ . +
                  NO2) # slope of NO2

################## POLLUTANTS SINGLY ##################
fml0alt = update(fml0, ~ . +
                   family_SES  +
                   place_birth +
                   civst +
                   residence +
                   birth_year +
                   age_diag +
                   sex +
                   f(birth_year2,age_diag,model="iid") +
                   f(birth_year3,sex,model="iid") +
                   f(age_diag2,sex,model="iid"))

# NOX
fml15 =  update(fml0alt, ~ . +
                  NOX) # slope of NOX

# CO
fml16 =  update(fml0alt, ~ . +
                  CO) # slope of CO

# EC
fml17 =  update(fml0alt, ~ . +
                  EC) # slope of EC

# O3
fml18 =  update(fml0alt, ~ . +
                  O3) # slope of O3

# NO2
fml19 =  update(fml0alt, ~ . +
                  NO2) # slope of NO2

################## BAYESIAN (R-INLA) CONDITIONAL LOGISTIC (fmls 20 - 34) ##################

# Intermediate fml for Bayesian conditional logistic models
fmlcl =  ck ~
  family_SES +
  place_birth +
  civst +
  residence +
  f(nr,model="iid",hyper=list(theta=list(initial=log(1e-6),fixed=T)))

# Conditional logistic for PM2.5 only
fml20 = update(fmlcl, ~ . +
                 PM25)

# Conditional logistic for NOX only
fml21 = update(fmlcl, ~ . +
                 NOX)

# Conditional logistic for CO only
fml22 = update(fmlcl, ~ . +
                 CO)

# Conditional logistic for EC only
fml23 = update(fmlcl, ~ . +
                 EC)

# Conditional logistic for O3 only
fml24 = update(fmlcl, ~ . +
                 O3)

# Conditional logistic for PM.25 + NOX
fml25 = update(fmlcl, ~ . +
                 PM25 + NOX)

# Conditional logistic for PM.25 + CO
fml26 = update(fmlcl, ~ . +
                 PM25 + CO) 

# Conditional logistic for PM.25 + EC
fml27 = update(fmlcl, ~ . +
                 PM25 + EC) 

# Conditional logistic for PM.25 + O3 
fml28 = update(fmlcl, ~ . +
                 PM25 + O3) 

# Conditional logistic for PM.25 + NO2 + CO
fml29 = update(fmlcl, ~ . +
                 PM25 + NOX + CO)

# Conditional logistic for PM25 + NOX + EC
fml30 = update(fmlcl, ~ . +
                 PM25 + NOX + EC)

# Conditional logistic for PM25 + NOX + O3
fml31 = update(fmlcl, ~ . +
                 PM25 + NOX + O3)

# Conditional logistic for PM25 + NOX + CO + EC
fml32 = update(fmlcl, ~ . +
                 PM25 + NOX + CO + EC) 

# Conditional logistic for PM25 + NOX + CO + O3
fml33 = update(fmlcl, ~ . +
                 PM25 + NOX + CO + O3) 

# Conditional logistic for PM25 + NOX + CO + EC + O3
fml34 = update(fmlcl, ~ . +
                 PM25 + NOX + CO + EC + O3)

########################################################################
# FREQUENTIST CONDITIONAL LOGISTIC MODELS
########################################################################

################## FREQUENTIST CONDITIONAL LOGISTIC PT. 1 (fmls 35 - 40) ##################

# Intermediate fml for Frequentist conditional logistic models
fmlcl2 = ck ~ 
  family_SES +
  place_birth +
  civst +
  residence

# Conditional logistic for PM2.5 only
fml35 = update(fmlcl2, ~ . +
                 PM25) # slope of PM2.5

# Conditional logistic for NOX only
fml36 = update(fmlcl2, ~ . +
                 NOX) # slope of NOX

# Conditional logistic for CO only
fml37 = update(fmlcl2, ~ . +
                 CO) # slope of CO

# Conditional logistic for EC only
fml38 = update(fmlcl2, ~ . +
                 EC) # slope of EC

# Conditional logistic for O3 only
fml39 = update(fmlcl2, ~ . +
                 O3) # slope of O3

# Conditional logistic for all pollutants
fml40 = update(fmlcl2, ~ . +
                 PM25 + CO + EC + O3 + NOX) # slopes of all included pollutants

################## BAYESIAN HIERARCHICAL CONDITIONAL LOGISTIC PT. 2 (fmls 41) ##################

# adding dummy variables
add_dummy_variables = function(data) {
  data$traffic.dummy = as.factor(NA)
  data$NOX.dummy = 'NOX.dummy'
  data$CO.dummy = 'CO.dummy'
  data$EC.dummy = 'EC.dummy' 
  return(data)
}

data_1yr = add_dummy_variables(data_1yr)
data_5yrs = add_dummy_variables(data_5yrs)
data_10yrs = add_dummy_variables(data_10yrs)

# Conditional logistic with correlated traffic-related effects
fml41 = update(fmlcl, ~ . +
                  PM25 + O3 + 
                  f(CO.dummy, CO, model="iid3d", n = 3 * 1, values = c('CO.dummy','EC.dummy','NOX.dummy')) + # n = 3 * 1 as there is one random effect per the 3d's
                  f(EC.dummy, EC, copy="CO.dummy") +
                  f(NOX.dummy, NOX, copy="CO.dummy"))

################## BAYESIAN HIERARCHICAL CONDITIONAL LOGISTIC PT. 2 (fml 42) ##################

# Conditional logistic for all pollutants but with STAN *rstanarm so not flexible)
# From http://mc-stan.org/rstanarm/reference/stan_clogit.html
fml42 = ck ~
  family_SES +
  place_birth +
  civst +
  residence +
  PM25 + CO + EC + O3 + NOX

################## FREQUENTIST CONDITIONAL LOGISTIC PT. 2 (fml 43) ##################

# Intermediate fml for Frequentist conditional logistic models
fmlcl3 = ck ~ 
  family_SES +
  place_birth +
  civst +
  residence + 
  splines::ns(parish_ses, df=3)

# Conditional logistic for all pollutants
fml43 = update(fmlcl3, ~ . +
                 PM25 + CO + EC + O3 + NOX) # slopes of all included pollutants

################## BAYESIAN HIERARCHICAL CONDITIONAL LOGISTIC PT. 3 (fml 44) ##################

# Conditional logistic for all pollutants but with STAN *rstanarm so not flexible)
# From http://mc-stan.org/rstanarm/reference/stan_clogit.html
fml44 = ck ~
  family_SES +
  place_birth +
  civst +
  residence +
  splines::ns(parish_ses, df=3) +
  PM25 + CO + EC + O3 + NOX

################## UNIFINSIHED BAYESIAN HIERARCHICAL CONDITIONAL LOGISTIC MODELS (fmls XX - XX) ##################
               
# from https://groups.google.com/g/r-inla-discussion-group/c/z3Ziic8WRMg
 
# Conditional logistic for three traffic-related pollutants and PM2.5
fmlXX = update(fmlcl, ~ . +
                  f(traffic.dummy, model="iid", values = c('CO.dummy','EC.dummy','NOX.dummy')) +
                  f(CO.dummy, CO,  copy="traffic.dummy") +
                  f(EC.dummy, EC,  copy="traffic.dummy") +
                  f(NOX.dummy, NOX,  copy="traffic.dummy") +
                  O3 + PM25)
               
# Conditional logistic for three traffic-related pollutants with hierarchy on traffic-related pollutants
fmlXX = update(fmlcl, ~ . +
                PM25 + O3 + f(Traffic, model="iid", n=3))
          

# Conditional logistic for three pollutants with hierarchy on traffic-related pollutants (needs scaled values and long table) TO FINISH
fmlXX = update(fmlcl, ~ . +
                 PM25 + O3 + Traffic + f(Traffic2,Value,model='iid'))

