library(epitrix)
library(outbreaker2)



# GENERATION AND INCUBATION DISTRIBUTIONS ---------------------------------


# Incubation Period
incub_mu <- 5.7
incub_sd <- 3.5 # Tan et al 
incub_cv <- incub_sd/incub_mu

incub_params <- gamma_mucv2shapescale(incub_mu, incub_cv) #get shape and scale from mu and cv
incub_params

#build discretised gamma distribution
incub <- distcrete::distcrete("gamma",
                              shape = incub_params$shape,
                              scale = incub_params$scale,
                              interval = 1, #day
                              w = 0.5 ) # How to weight the endpoints;
#must be between 0 and 1. 
#If 0.5 then integration happens centred 
#around the interval, 
#if 0 floor, if 1 then ceiling.) 


tibble(days = 0:20,p = incub$d(0:20)) %>% 
  ggplot()+
  aes(x = days, y = p)+
  geom_col(width = 0.95, fill = "steelblue")+
  scale_x_continuous(breaks = c(0:10))+
  theme_minimal()+
  labs(title = "incubation period")


#GENERATION TIME

# Incubation Period
gi_mu <- 3.95
gi_sd <- 1.51 #Ganyani et al~
gi_cv <- gi_sd/gi_mu

gi_params <- gamma_mucv2shapescale(gi_mu, gi_cv)
gi_params

#build discretised gamma distribution
gi <- distcrete::distcrete("gamma",
                           shape = gi_params$shape,
                           scale = gi_params$scale,
                           interval = 1, #day
                           w = 0.5 ) # How to weight the endpoints; must be between 0 and 1. If 0.5 then integration happens centred around the interval, if 0 floor, if 1 then ceiling.) 


tibble(days = 0:10,
       p = gi$d(0:10)) %>% 
  ggplot()+
  aes(x = days, y = p)+
  geom_col(width = 0.95,fill = "steelblue")+
  scale_x_continuous(breaks = c(0:10))+
  theme_minimal()+
  labs(title = "generation time")





# DATA --------------------------------------------------------------------
test_df <- df %>% 
  filter(household_id == "40638") #select one example household #19062 #40638            
test_df

dates <- test_df$start_dt2 %>% 
  sort() #dates of symptom onset
dates


out_data <- outbreaker_data(
  dates = dates, # dates of onset
  ids = test_df$newID,
  w_dens = gi$d(0:100), #generation time distribution
  f_dens = incub$d(0:100) # incubation period
  )
out_data

config <- create_config(
  move_kappa = FALSE, #do not look for missing cases
  move_pi = FALSE, #reporting rate
  move_mu = FALSE, #mutation rate
  init_kappa = 1, #number of generations before the last sampled ancestor
  init_pi = 1, #100% of reporting rate = all cases are reported
  find_import = FALSE #no imported cases
)


set.seed(12)

out <- outbreaker(data = out_data, 
                  config = config)  
summary(out)
summary(out, burnin = 500)
View(out)
?outbreaker_chains
?plot.outbreaker_chains

plot(out) # trace of the log-posterior densitites Explain?~
plot(out, burn = 500) #run for 2x long subsample 2x more
acf(out$post[-(1:10)]) #look into effective sample size to have a grasp of w inf whom #autocorrelation sample 5 times more to lower acf correlation of the chain with itself + correlation of the chain with lag(by1) etc..
plot(out, "prior") #why is the prior at that value, what does it mean?
plot(out, "mu") #mutation rate = 0 OK
plot(out, "t_inf_5") #is the digit for the ID? Interpret plot? 
plot(out, "t_inf_1") 
plot(out, type = "alpha", labels = out_data$ids)# infectors
plot(out, type = "t_inf", labels = out_data$ids) # dates of infection
plot(out, type = "kappa", labels = out_data$ids) # generations
plot(out, type = "network", min_support = 0.2, labels = out_dat$ids) 
summary(out)$tree 
