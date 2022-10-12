# functions to process model results

# process R-STAN
process_STAN = function(fml, dataset, pollutant){
  
  # load model output
  model_current <- paste0(output.folder,'mod','_',fml,'_',dataset,'.rds')
  print(paste0('Processing ',model_current))
  model_current <- readRDS(model_current)
  draws <- rstan::extract(model_current)
  
  # extract parameter fits
  if(pollutant%in%c('NOX','NO2')){ # only ever run model either NOX or NO2
    fit_summary=summary(model_current, pars=c('beta_traffic_exp[1]'), probs=c(0.025, 0.975))$summary
    draws_single = draws$beta_traffic_exp[,1]
  }
  if(pollutant=='CO'){
    if(fml!='fml104'){
    fit_summary=summary(model_current, pars=c('beta_traffic_exp[2]'), probs=c(0.025, 0.975))$summary
    draws_single = draws$beta_traffic_exp[,2]
    }
    if(fml=='fml104'){
      fit_summary=summary(model_current, pars=c('beta_traffic_exp[1]'), probs=c(0.025, 0.975))$summary
      draws_single = draws$beta_traffic_exp[,1]
    }  
  }
  if(pollutant=='EC'){
    if(fml!='fml105'){
      fit_summary=summary(model_current, pars=c('beta_traffic_exp[3]'), probs=c(0.025, 0.975))$summary
      draws_single = draws$beta_traffic_exp[,3]
    }
    if(fml=='fml105'){
      fit_summary=summary(model_current, pars=c('beta_traffic_exp[1]'), probs=c(0.025, 0.975))$summary
      draws_single = draws$beta_traffic_exp[,1]
    }  
  }
  if(pollutant=='PM25'){
    fit_summary=summary(model_current, pars=c('beta_nontraffic_exp[1]'), probs=c(0.025, 0.975))$summary
    draws_single = draws$beta_nontraffic_exp[,1]
  }
  if(pollutant=='O3'){
    if(fml!='fml59'){
      fit_summary=summary(model_current, pars=c('beta_nontraffic_exp[2]'), probs=c(0.025, 0.975))$summary
      draws_single = draws$beta_nontraffic_exp[,2]
    }
    if(fml=='fml59'){
      fit_summary=summary(model_current, pars=c('beta_nontraffic_exp'), probs=c(0.025, 0.975))$summary
      draws_single = draws$beta_nontraffic_exp[,1]
    }
  }
  if(pollutant=='Traffic Total'){
    fit_summary=summary(model_current, pars=c('phi'), probs=c(0.025, 0.975))$summary
    draws_single = draws$phi
  }
  if(pollutant=='Traffic Average'){
    fit_summary=summary(model_current, pars=c('lambda_exp'), probs=c(0.025, 0.975))$summary
    draws_single = draws$lambda_exp
  }
  
  odds.mean <- as.numeric(fit_summary[,'mean']) - 1 
  odds.ll <- as.numeric(fit_summary[,'2.5%']) - 1  
  odds.ul <- as.numeric(fit_summary[,'97.5%']) - 1 
  odds.prob <- sum(draws_single>1)/length(draws_single)
  dat.temp <- data.frame(stats='Bayesian',pollutant = pollutant, mod=fmls[i], data=datasets[j],odds.mean=odds.mean,odds.ll=odds.ll,odds.ul=odds.ul,odds.prob=odds.prob)
  
  return(dat.temp)
  
}

process_STANARM = function(fml, dataset, pollutant){
  
  # load model output
  model_current <- paste0(output.folder,'mod','_',fml,'_',dataset,'.rds')
  print(paste0('Processing ',model_current))
  model_current <- readRDS(model_current)
  
  # currently working from below links:
  # https://nwfsc-timeseries.github.io/atsa-labs/sec-uss-fitting-with-stan.html
  # https://www.weirdfishes.blog/blog/fitting-bayesian-models-with-stan-and-r/
  # extract parameter fits
  fit_summary = model_current$stan_summary[pollutant,]
  
  odds.mean <- exp(as.numeric(fit_summary[['mean']])) - 1 
  odds.ll <- exp(as.numeric(fit_summary[['2.5%']])) - 1  
  odds.ul <- exp(as.numeric(fit_summary[['97.5%']])) - 1 
  odds.prob <- NA # TO DO 
  dat.temp <- data.frame(stats='Bayesian',pollutant = pollutant, mod=fmls[i], data=datasets[j],odds.mean=odds.mean,odds.ll=odds.ll,odds.ul=odds.ul,odds.prob=odds.prob)
  
  return(dat.temp)
  
}

##### unused below ##### 

# process Bayesian R-INLA
process_INLA = function(fml, dataset, pollutant){
  
  # load model output
  model_current <- paste0(output.folder,'mod','_',fml,'_',dataset,'.rds')
  print(paste0('Processing ',model_current))
  model_current <- readRDS(model_current)
  
  # find credible intervals and probability of positive association
  if(pollutant=='Traffic')
  {marginal.exp <- inla.tmarginal(function(x) exp(x), model_current$marginals.lincomb.derived[['lc']])}
  if(pollutant!='Traffic'& fml!='fml41')
  {marginal.exp <- inla.tmarginal(function(x) exp(x), model_current$marginals.fixed[[pollutant]])}
  if(pollutant!='Traffic'& fml=='fml41'){ # for special multi-pollutant with correlated output
    if(pollutant %in% c('PM25','O3'))
    {marginal.exp <- inla.tmarginal(function(x) exp(x), model_current$marginals.fixed[[pollutant]])}
    if(pollutant %in% c('CO','EC','NOX'))
    { pollutant.dummy=paste0(pollutant,'.dummy')
    marginal.exp <- inla.tmarginal(function(x) exp(x), model_current$marginals.random[[pollutant.dummy]]$index.1)
    }
  }
  odds.mean <- inla.emarginal(function(x) x, marginal.exp) - 1
  odds.ll <- inla.qmarginal(0.025, marginal.exp) - 1
  odds.ul <- inla.qmarginal(0.975, marginal.exp) - 1
  odds.prob <- 1 - inla.pmarginal(1, marginal.exp)
  dat.temp <- data.frame(stats='Bayesian',pollutant = pollutant, mod=fmls[i], data=datasets[j],odds.mean=odds.mean,odds.ll=odds.ll,odds.ul=odds.ul,odds.prob=odds.prob)
  
  return(dat.temp)
  
}

# process Bayesian R-INLA for traffic-related pollutant combination
process_INLA_2 = function(fml, dataset, traffic_related_pollutants, draws){
  
  # load model output
  model_current <- paste0(output.folder,'mod','_',fml,'_',dataset,'.rds')
  print(paste0('Processing ',model_current))
  model_current <- readRDS(model_current)
  
  # find credible intervals and probability of positive association
  dat.temp = data.frame()
  for(pollutant in traffic_related_pollutants){
    pollutant = gsub("\\:", "$index.", pollutant)
    pollutant_name = sub("\\$.*", "", pollutant) ; pollutant_index = sub(".*\\$", "", pollutant)
    marginal = model_current$marginals.random[[pollutant_name]][[pollutant_index]]
    marginal.exp <- inla.tmarginal(function(x) exp(x), marginal)
    odds.mean <- inla.emarginal(function(x) x, marginal.exp) - 1
    odds.ll <- inla.qmarginal(0.025, marginal.exp) - 1
    odds.ul <- inla.qmarginal(0.975, marginal.exp) - 1
    odds.prob <- 1 - inla.pmarginal(1, marginal.exp)
    dat.temp.current <- data.frame(stats='Bayesian',pollutant = sub("\\..*", "", pollutant), mod=fmls[i], data=datasets[j],odds.mean=odds.mean,odds.ll=odds.ll,odds.ul=odds.ul,odds.prob=odds.prob)
    
    dat.temp.current = merge(dat.temp.current, iqr_dataframe, by='pollutant')
    dat.temp.current$odds.iqr.mean = dat.temp.current$odds.mean * dat.temp.current$iqr
    dat.temp.current$odds.iqr.ll = dat.temp.current$odds.ll * dat.temp.current$iqr
    dat.temp.current$odds.iqr.ul = dat.temp.current$odds.ul * dat.temp.current$iqr
    dat.temp = rbind(dat.temp,dat.temp.current)
  }
  
  
  # make draws from posterior marginals for aim of examining combined effect
  draws_current = inla.posterior.sample(draws, model_current)
  
  # loop over draws and find total effect of traffic-related pollutants
  parameter.table = data.frame()
  for(draw in seq(draws)){ # draw = 1
    pollutant.values = draws_current[[draw]]$latent[grep(paste(traffic_related_pollutants,collapse="|"),rownames(draws_current[[draw]]$latent))]
    pollutant.values = data.frame(pollutant=sub("\\..*", "", traffic_related_pollutants),odds.mean=pollutant.values)  
    pollutant.values = merge(pollutant.values, iqr_dataframe, by='pollutant')
    
    pollutant.values.sum = sum(pollutant.values$odds.mean)
    pollutant.values.sum.exp = exp(pollutant.values.sum) - 1   
    
    pollutant.values$odds.iqr.mean = with(pollutant.values, odds.mean * iqr)
    pollutant.values.sum.iqr = sum(pollutant.values$odds.iqr.mean)
    pollutant.values.sum.exp.iqr = exp(pollutant.values.sum.iqr) - 1
    
    table = data.frame(draw=draw, odds=pollutant.values.sum.exp, odds.iqr=pollutant.values.sum.exp.iqr)
    parameter.table = rbind(parameter.table,table)
  }
  
  # calculate the mean and the Credible Interval of effect
  odd.mean = mean(parameter.table$odds)
  odds.ll = quantile(parameter.table$odds,0.025)
  odds.ul = quantile(parameter.table$odds,0.975)
  odds.iqr.mean = mean(parameter.table$odds.iqr)
  odds.iqr.ll = quantile(parameter.table$odds.iqr,0.025)
  odds.iqr.ul = quantile(parameter.table$odds.iqr,0.975)
  odds.prob = nrow(subset(parameter.table,odds.iqr>0)) / nrow(parameter.table)
  dat.temp.current<- data.frame(stats='Bayesian',pollutant = 'Traffic', mod=fmls[i], data=datasets[j],odds.mean=odds.mean,odds.ll=odds.ll,odds.ul=odds.ul,odds.prob=odds.prob,
                                iqr=NA, odds.iqr.mean=odds.iqr.mean,odds.iqr.ll=odds.iqr.ll,odds.iqr.ul=odds.iqr.ul)
  
  # combine individual values with combined traffic value
  dat.temp = dplyr::bind_rows(dat.temp,dat.temp.current)
  
  return(dat.temp)
  
}

# process frequentist clogit function with errors
process_clogit = function(fml, dataset, pollutant){
  
  # load model output
  model_current <- paste0(output.folder,'mod','_',fml,'_',dataset,'.rds')
  print(paste0('Processing ',model_current))
  model_current <- readRDS(model_current)
  
  # parameter of interest
  beta <- model_current$coefficients[[pollutant]]
  beta.se <- sqrt(model_current$var[as.character(pollutant),as.character(pollutant)])
  
  # exponentiate coef and 95% CI
  
  odds.mean <- exp(beta) - 1
  odds.ll <- exp(beta - 1.96 * beta.se) - 1
  odds.ul <- exp(beta + 1.96 * beta.se) - 1
  odds.prob <- NA
  dat.temp <- data.frame(stats='Freq',pollutant = pollutant, mod=fmls[i], data=datasets[j],odds.mean=odds.mean,odds.ll=odds.ll,odds.ul=odds.ul,odds.prob=odds.prob)
  
  return(dat.temp)
}

# process frequentist clogit function with errors for Greenland sensitivity
process_clogit_gl = function(mod, pollutant){
  
  model_current = get(mod)
  
  # parameter of interest
  beta <- model_current$coefficients[[pollutant]]
  beta.se <- sqrt(model_current$var[as.character(pollutant),as.character(pollutant)])
  
  # exponentiate coef and 95% CI
  
  odds.mean <- exp(beta) - 1
  odds.ll <- exp(beta - 1.96 * beta.se) - 1
  odds.ul <- exp(beta + 1.96 * beta.se) - 1
  dat.temp <- data.frame(stats='Freq',pollutant = pollutant, mod=mod,odds.mean=odds.mean,odds.ll=odds.ll,odds.ul=odds.ul)
  
  return(dat.temp)
}