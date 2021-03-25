model{
  #Priors
  a0 ~ dnorm(0, 0.001)
  a1 ~ dnorm(0, 0.001)
  a2 ~ dnorm(0, 0.001)
  a3 ~ dnorm(0, 0.001)
  a4 ~ dnorm(0, 0.001)
  a5 ~ dnorm(0, 0.001)
  b0 ~ dnorm(0, 0.001)
  b1 ~ dnorm(0, 0.001)
  b2 ~ dnorm(0, 0.001)
  b3 ~ dnorm(0, 0.001)
  t0 ~ dnorm(0, .01) 
  tau <- exp(t0)
  
  
  #likelihood
  #C <- 10000
  for(i in 1:n.sites){
    y[i] ~ dbern(alpha[i])
    logit(alpha[i]) <- a0 + a1*comp.abs[i] + a2*canopy.abs[i]+ a3*hemlock.abs[i] + a4*cedar.abs[i] + a5*open_cover.abs[i]
  }
  
  for(i in 1:n.cont){
    y.c[i] ~ dbeta(p[i],q[i])
    p[i] <- mu2[i] * tau
    q[i] <- (1 - mu2[i]) * tau
    logit(mu2[i]) <- b0 + b1*canopy.dens[i] + b2*comp.dens[i] + b3*aspect.dens[i]
  }
  
}
    
    
    
   #  pres[i] ~ dbin(p[i],1)
   # # b[i] ~ dnorm(0, tau)
   #  logit(p[i]) <- a0 #+ b[i]
   #  
   #  Y[i] ~ dgamma(mu[i], mu[i])
   #  mu[i] <- (b0 + b1*aspect[i]) / p[i]
   #  
    
    
    # logit(w[i]) <- z[i]
    # z[i] <- a0
    # 
    # mu[i] <- pow(eta[i], -1)
    # eta[i] <- b0 + b1*aspect[i] + b2*comp[i]
    # 
    # shape[i] <- pow(mu[i], 2) / pow(sd, 2)
    # rate[i] <- mu[i] / pow(sd, 2)
    # 
    # logGamma[i] <- log(dgamma(Y[i], shape[i], rate[i]))
    # 
    # logLik[i] <- (1 - z[i]) * log(1 - w[i]) + z[i] * ( log(w[i]) + logGamma[i] )
    # 
    # Lik[i] <- exp(logLik[i])
    # 
    # # Use the ones trick
    # p[i] <- Lik[i] / C
    # ones[i] ~ dbern(p[i])
    
    
    #Y[i] ~  dbeta(mu[i],(1-mu[i]))
    #logit(mu[i]) = b0 + b1*aspect[i] + b2*comp[i]





