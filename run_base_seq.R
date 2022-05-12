##---------------
# Clears all old variables
# rm(list=ls())

##---------------
# Sets the directory automatically
current_working_dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_working_dir)

source("utils.R")

rounds     = 250
companies  = 10 
strategy   = "none"
base_costs = c(101.08316,1.89282,65.77217)
base_delay = c(1.151e-01,7.456e-04,2.424e-03)
capital    = 100000
teams      = 3
fixed_cost = 250
cpr        = 5
# r          = 1
K          = 500 # simulacoes
lambda     = 1 #seq(0,2,.5)
s2p        = 0.005
s2q        = 0.25
generations= 100 # generations*10 empresas aleat?rias
best_gen   = matrix(nrow = generations, ncol = 4)

# start_time <- Sys.time()
# start_time + (a/10)*K
# for (lambda in c(seq(0,2,.5),10)){
# [1]  0.0  0.5  1.0  1.5  2.0 10.0
# for (lambda in 0){
# for (lambda in 0.5){

lambda = 2

for (lambda in c(seq(0,2,.5),10)){
  
  start_time <- Sys.time()
  print(paste("lambda = ",lambda))
  # print(start_time + a)
  
  # set.seed(2022)
  set.seed(202202)
  
  resultsGen   = matrix(nrow = K, ncol = 4)
  
  for (k in 1:K){
    for (i in 1:generations){# 1000 empresas divididas em blocos de 10, ou seja, 100 blocos de 10
      gen_vector = matrix(runif(10*4),ncol = 4)
      
      market = testStrategies(rounds     = rounds,
                              fixed_cost = fixed_cost, 
                              teams      = teams,
                              lambda     = lambda,
                              gen_vector = gen_vector)
      
      best   = which.max(market$balance[rounds,])
      best_gen[i,] = gen_vector[best,]
    }
    #Agora temos 100 empresas
    
    while (nrow(best_gen) > 10){
      
      generations  = generations/companies # divide por bloco de 10
      best_gen_tmp = matrix(nrow = generations, ncol = 4)
      
      for (i in 1:generations){
        gen_vector = best_gen[((i-1)*companies+1):(i*companies),]
        
        market = testStrategies(rounds     = rounds,
                                fixed_cost = fixed_cost, 
                                teams      = teams,
                                lambda     = lambda,
                                gen_vector = gen_vector)
        
        best   = which.max(market$balance[rounds,])
        best_gen_tmp[i,] = gen_vector[best,]
      }
      best_gen = best_gen_tmp
    }
    #Agora temos 10 empresas, ou seja, 1 bloco
    
    market = testStrategies(rounds     = rounds,
                            fixed_cost = fixed_cost, 
                            teams      = teams,
                            lambda     = lambda,
                            gen_vector = best_gen)
    

    gens = best_gen[which.max(market$balance[rounds,]),]
    # print(gens)
    resultsGen[k,] = gens
  }
  # print(paste("lambda = ",lambda))
  print(" ")
  resultsGen = as.data.frame(resultsGen)
  names(resultsGen) = c("G1","G2","G3","G4")
  write.table(resultsGen, paste0("results_2_lambda_",lambda,"_K_",K,".txt"),
              append = FALSE, sep = ",",row.names = FALSE)
  print(end_time <- Sys.time())
  Sys.sleep(3600)
}
# end_time <- Sys.time()
# end_time - start_time
