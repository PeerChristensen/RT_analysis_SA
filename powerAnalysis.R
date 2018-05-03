#power analyses
library(pwr)
library(lme4)
library(lmerTest)
library(simr)


pwr.f2.test(u = 3,f2 = 0.3/(1-0.3), sig.level = 0.05, power = 0.8)

#n = v + u + 1 = 23 + 2 + 1 = 26


d = expand.grid(subject=rep(1:90,each=160), 
                condition=c(rep("size"),rep("height")))  
d$RT = numeric(nrow(d))
d$congruent = rep(c(T,F),nrow(d)/2)
d$language = rep(c("X","Y","Z"),each=160)

d$RT[d$condition=="size"] = rnorm(nrow(d[d$condition=="size",]), mean=420, sd=100)   

d$RT[d$condition=="height"] = rnorm(nrow(d[d$condition=="height",]), mean=430, sd=100)  

fit =lmer(RT ~ condition+congruent+condition:language+(1|subject),data=d)
summary(fit)


powerSim(fit, alpha=0.05) 


#####
#1 random effect
nParticipants = 14
nObs = 320
mu = 10
sds = 2
sd = 1

participant = rep(LETTERS[1:nParticipants], each = nObs) 

randEff = rnorm(nParticipants, 0, sds)
randEff = rep(randEff, each = nObs)

obsEff = rnorm(nParticipants*nObs, 0, sd)
dat = data.frame(participant, randEff, obsEff)

dat$outcome = with(dat, mu + randEff + obsEff ) 

fit1 = lmer(outcome ~ 1 + (1|participant), data = dat)
fit1

#simulation function
twolevel_fun = function(nParticipants = 20, nObs = 320, mu = 10, sigma_s = 2, sigma = 1) {
  randEff = rep( rnorm(nParticipants, 0, sigma_s), each = nObs)
  participant = rep(1:nParticipants, each = nObs)
  obsEff = rnorm(nParticipants*nObs, 0, sigma)
  outcome = mu + randEff + obsEff
  dat = data.frame(participant, outcome)
  lmer(outcome ~ 1 + (1|participant), data = dat)
}

set.seed(16)
twolevel_fun()

sims = replicate(100, twolevel_fun() )
sims[[100]]

library(purrr) # v. 0.2.4
suppressPackageStartupMessages( library(dplyr) ) # v. 0.7.4
library(ggplot2) # v. 2.2.1

participant_sims = c(10, 15, 25) %>%
  set_names() %>%
  map(~replicate(100, twolevel_fun(nParticipants = .x) ) )

participant_vars = participant_sims %>%
  modify_depth(2, ~tidy(.x, effects = "ran_pars", scales = "vcov") ) %>%
  map_dfr(bind_rows, .id = "participant_num") %>%
  filter(group == "participant")
head(participant_vars)

ggplot(participant_vars, aes(x = estimate) ) +
  geom_density(fill = "blue", alpha = .25) +
  facet_wrap(~participant_num) +
  geom_vline(xintercept = 4)

participant_vars = mutate(participant_vars, participant_num = forcats::fct_inorder(participant_num) )

add_prefix = function(string) {
  paste("Number participants:", string, sep = " ")
}

groupmed = participant_vars %>%
  group_by(participant_num) %>%
  summarise(mvar = median(estimate) )

ggplot(participant_vars, aes(x = estimate) ) + 
  geom_density(fill = "blue", alpha = .25) +
  facet_wrap(~participant_num, labeller = as_labeller(add_prefix) ) +
  geom_vline(aes(xintercept = 4, linetype = "True variance"), size = .5 ) +
  geom_vline(data = groupmed, aes(xintercept = mvar, linetype = "Median variance"),
             size = .5) +
  theme_bw() +
  scale_linetype_manual(name = "", values = c(2, 1) ) +
  theme(legend.position = "bottom",
        legend.key.width = unit(.1, "cm") ) +
  labs(x = "Estimated Variance", y = NULL)

#two predictore
nParticipants = 14
nObs = 320
b0 = -1
b1 = -9
b2 = 4
sds = 2
sd = 1

one = rep( runif(nParticipants, 300, 600), each = nObs) 
two = runif(nParticipants*nObs, 300, 600) 

outcome2 = b0 + b1*one + b2*two + randEff + obsEff 

fit2 =lmer(outcome2 ~ one + two + (1|participant) )
summary(fit)


#####
#another method (not working)

library(plyr)
library(mvtnorm)
library(lme4)
make.data.generator <- function(true.effects=c(0,0),
                                resid.var=1,
                                ranef.covar=diag(c(1,1)),
                                n.subj=24,
                                n.obs=24)
{
  # create design matrix for our made up experiment
  data.str <- data.frame(freq=factor(c(rep('high', n.obs/2), rep('low', n.obs/2))))
  contrasts(data.str$freq) <- contr.sum(2)
  model.mat <- model.matrix(~ 1 + freq, data.str)
  generate.data <- function()
  {
    # sample data set under mixed effects model with random slope/intercepts
    simulated.data <- rdply(n.subj,
                            {
                              beta <- t(rmvnorm(n=1, sigma=ranef.covar)) + true.effects
                              expected.RT <- model.mat %*% beta
                              epsilon <- rnorm(n=length(expected.RT), mean=0, sd=sqrt(resid.var))
                              data.frame(data.str,
                                         RT=expected.RT + epsilon)
                            }
    )
    names(simulated.data)[1] <- 'subject'
    simulated.data
  }
}

fit.models <- function(simulated.data){
  # fit models and extract coefs
  lm.coefs <- coefficients(summary(lm(RT ~ 1+freq, simulated.data)))[, 1:3]
  rand.int.coefs <- coefficients(summary(lmer(RT ~ 1+freq + (1|subject), simulated.data)))
  rand.slope.coefs <- coefficients(summary(lmer(RT ~ 1+freq + (1+freq|subject), simulated.data)))
  # format output all pretty
  rbind(data.frame(model='lm', predictor=rownames(lm.coefs), lm.coefs),
        data.frame(model='rand.int', predictor=rownames(rand.int.coefs), rand.int.coefs),
        data.frame(model='rand.slope', predictor=rownames(rand.slope.coefs), rand.slope.coefs))
}

gen.dat <- make.data.generator(true.effects=c(0,0), n.subj=24, n.obs=24)
simulations <- rdply(.n=100,fit.models(gen.dat()))

