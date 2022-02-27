library(tidyverse)
library(broom.mixed)
library(fixest)
library(CUB)

Bigraph = list()

Bigraph$Users = tibble(User = str_c("User ",c(1:30)),
                                Psi = rbeta(30,3.3,1.38),
                                k = rpois(30,10)
)

Bigraph$Items = tibble(Item = str_c("Item ",c(1:6)),
                       Alpha = rbeta(6,16,9),
                       w = rbeta(6,8,8),
)

Bigraph$Ratings = tibble(
  User = c(rep(Bigraph$Users$User,Bigraph$Users$k))
) %>% rowwise() %>%
  mutate(Item = sample(Bigraph$Items$Item,1,replace = F),
         Psi = Bigraph$Users$Psi[Bigraph$Users$User == User],
         w = Bigraph$Items$w[Bigraph$Items$Item == Item],
         Alpha = Bigraph$Items$Alpha[Bigraph$Items$Item == Item],
         p = Psi*w + Alpha*(1-w),
         y = rbinom(1,10,p)
  )

Bigraph$Ratings$y %>% mean()
Bigraph$Ratings$y %>% sd()

# Problema numero 1: GEM non forza un print()
CUB::GEM(y ~ 0,
         data = Bigraph$Ratings,
         family = "cub")

CUB::GEM(y ~ 0,
         data = Bigraph$Ratings,
         family = "igh") %>% print()

### Problema 2, sebbene non accetti covariate nominali, restituisce delle stime

CUB::GEM(y ~ Item,
         data = Bigraph$Ratings,
         family = "cub") %>% print()

### Problema 2.1 ha senso un modello
### a dati strutturati/Multilevel/Mixed Effects?

feols(y ~ 0 + Item|(User),
      data = Bigraph$Ratings)

CUB::GEM(y ~ Item,
         data = Bigraph$Ratings,
         family = "cub") -> CUB_Bigraph

CUB_Bigraph$estimates

CUB::GEM(y ~ Item|User,
         data = Bigraph$Ratings,
         family = "cub") %>% print()

### Problema 3, mancata integrazione con Magritte

CUB::GEM(y ~ 0,
         data = Bigraph$Ratings,
         family = "cub") %>% print()

### Cos'è un tidier?

feols(y ~ 0 + Item|(User),
      data = Bigraph$Ratings) %>% tidy()
