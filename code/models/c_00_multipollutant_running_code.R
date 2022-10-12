# function to run frequentist conditional logistic
clogit_run = function(formula, data){
  mod = Epi::clogistic(formula, strata = nr, data = data)
  return(mod)
}

# function to run Bayesian conditional logistic in R-STAN ARM
clogit_stan_run = function(formula, data){
  mod = stan_clogit(formula, 
                    strata = nr, 
                    data = data,
                    QR=FALSE)
  return(mod)
}

### UNUSED BELOW FROM INLA ###

# function to run INLA model
inla_run = function(formula, data){
  mod = inla(formula, family='binomial', 
             control.family=list(link='logit'),
             control.predictor=list(link=1, compute=TRUE),
             control.compute=list(dic=TRUE),
             data = data)
  
  return(mod)
  
}

# function to run INLA model for conditional logistic specifically
inla_run_2 = function(formula, data){
  
  mod = inla(formula, family='Poisson', 
             control.family=list(link='log'),
             control.predictor=list(link=1, compute=TRUE),
             control.compute=list(dic=TRUE, config=TRUE),
             data = data)
  
  return(mod)
  
}

# function to run INLA model for conditional logistic specifically with linear combination of traffic-related pollutants
inla_run_3 = function(formula, data, lin.1, lin.2, lin.3){
  
  # linear combination of the traffic-related pollutants
  traffic.lc = inla.make.lincomb(CO=lin.1, EC=lin.2 , NOX=lin.3)
  
  mod = inla(formula, family='Poisson',
             control.family=list(link='log'),
             control.predictor=list(link=1, compute=TRUE),
             control.compute=list(dic=TRUE, config=TRUE),
             lincomb = traffic.lc,
             data = data)
  
  return(mod)
  
}

# function to run INLA model for conditional logistic specifically with linear combination of traffic-related pollutants using correlated structure
inla_run_4 = function(formula, data, lin.1, lin.2, lin.3){
  
  # linear combination of the traffic-related pollutants
  traffic.lc = inla.make.lincomb(CO.dummy=lin.1, EC.dummy=lin.2 , NOX.dummy=lin.3)
  
  mod = inla(formula, family='Poisson',
             control.family=list(link='log'),
             control.predictor=list(link=1, compute=TRUE),
             control.compute=list(dic=TRUE, config=TRUE),
             lincomb = traffic.lc,
             data = data)
  
  return(mod)
  
}

# function to run INLA model for conditional logistic specifically with linear combination of traffic-related pollutants using stack (not currently used)
inla_run_5 = function(formula, data, lin.1, lin.2, lin.3){
  
  # inla stack object 
  stack <- inla.stack(data=list(ck=data$ck), 
                      A=list(cbind(data$CO, data$EC,data$NOX),1), 
                      effects = list(list(Traffic=1:3),
                                     list(family_SES=data$family_SES, place_birth=data$place_birth,
                                          civst=data$civst, residence=data$residence, 
                                          nr=data$nr, PM25=data$PM25, O3=data$O3)))
  
  traffic.lc = inla.make.lincomb(Traffic = c(lin.1,lin.2,lin.3))
  
  mod = inla(formula, family='Poisson',
             control.family=list(link='log'),
             control.predictor=list(link=1, compute=TRUE,A=inla.stack.A(stack)),
             control.compute=list(dic=TRUE, config=TRUE),
             lincomb = traffic.lc,
             data = inla.stack.data(stack))
  
  return(mod)
  
}
