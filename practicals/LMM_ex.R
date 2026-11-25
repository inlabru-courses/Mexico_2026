## -----------------------------------------------------------------------------
#| warning: false
#| message: false
#| code-summary: "Load libraries"

library(INLA)
library(patchwork)
library(inlabru)
library(tidyverse)
# load some libraries to generate nice plots
library(scico)


## -----------------------------------------------------------------------------
#| echo: true

bru_options_set(control.compute = list(dic = T, waic = T))




## -----------------------------------------------------------------------------
#| code-fold: show
#| code-summary: "Get body_mass data"

data("penguins")
glimpse(penguins)




## -----------------------------------------------------------------------------
penguins =  penguins %>% select(c(species,flipper_len,body_mass,sex))


## -----------------------------------------------------------------------------
penguins$body_mass = penguins$body_mass/1000


## -----------------------------------------------------------------------------
penguins = penguins %>% drop_na(-body_mass)










## -----------------------------------------------------------------------------
#| eval: true

fit1 = bru(cmp, lik)



## -----------------------------------------------------------------------------
#| code-summary: "Model summaries"
#| collapse: true
summary(fit1)




## -----------------------------------------------------------------------------
new_data = data.frame(flipper_len  = 170:240)

pred = predict(fit1, new_data, ~ beta0 + beta1,
               n.samples = 1000)


## -----------------------------------------------------------------------------
#| code-fold: true
#| fig-cap: Data and 95% credible intervals
#| message: false
#| warning: false
#| fig-align: center
#| fig-width: 4
#| fig-height: 4

pred %>% ggplot() +
  geom_line(aes(flipper_len,mean)) +
  geom_ribbon(aes(flipper_len, ymin = q0.025, ymax = q0.975), alpha = 0.5) +
  xlab("Flipper length") + ylab("body_mass") +
  geom_point(data = penguins, aes(flipper_len, body_mass))





## -----------------------------------------------------------------------------
#| code-fold: true
penguins %>%
  ggplot() + geom_point(aes(flipper_len, body_mass, color= sex)) +
  facet_wrap(.~sex)


## -----------------------------------------------------------------------------
#| eval: true

cmp = ~ -1 + effects( ~ sex , model = "fixed")

formula = body_mass ~ effects

lik = bru_obs(formula = formula,
              data = penguins
              )
fit2 = bru(cmp, lik)





## -----------------------------------------------------------------------------
#| eval: true

cmp = ~ -1 + effects( ~ sex*flipper_len, model = "fixed")
formula = body_mass ~ effects
lik = bru_obs(formula = formula,
              data = penguins
              )
fit3 = bru(cmp, lik)




## -----------------------------------------------------------------------------
fit3$summary.random




## -----------------------------------------------------------------------------
#| echo: true
#| code-fold: true
penguins %>%
  ggplot() +
  geom_point(aes(flipper_len, body_mass, color = species)) +
  facet_wrap(.~sex)



## -----------------------------------------------------------------------------
#| echo: true
#| eval: false
# cmp = ~ -1 + effects( ~ sex*flipper_len, model = "fixed") +
#   species(species, ... )
# formula = ...
# lik = bru_obs(formula = formula,
#               data = penguins
#               )
# fit4 = bru(cmp, lik)




## -----------------------------------------------------------------------------
fit4$summary.random$effects[,c(1,3,5)]
fit3$summary.random$effects[,c(1,3,5)]



## -----------------------------------------------------------------------------
#| echo: true
#| code-fold: true

# Predict
pred = predict(fit4, penguins, formula = ~ effects + species)

pred %>%
  ggplot(aes(x=flipper_len,y=mean,color=factor(sex)))+
  geom_line()+
  geom_ribbon(aes(flipper_len,ymin = q0.025, ymax= q0.975,fill=factor(sex)), alpha = 0.5) +
  geom_point(data=penguins,aes(x=flipper_len,y=body_mass,colour=factor(sex)))+
  facet_wrap(~species,scales="free_x")



## -----------------------------------------------------------------------------
#| echo: true
#| eval: false

# 
# cmp = ~ -1 + effects( ~ sex*flipper_len, model = "fixed") +
#   species1(species, ... ) +
#   species2(species, ... )
# 
# formula = ...
# 
# lik = bru_obs(formula = formula,
#               data = penguins
#               )
# 
# fit5 = bru(cmp, lik)




## -----------------------------------------------------------------------------
deltaIC(fit1, fit2, fit3, fit4, fit5, criterion = c("DIC","WAIC"))


