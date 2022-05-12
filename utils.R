# Read data
data     = readRDS(file = "data.rds")
model    = readRDS(file = "model.rds")
library(truncnorm)

# start the game with initial assumptions
testStrategies <- function(rounds     = 1250, 
                           companies  = 10, 
                           base_costs = c(101.08316,1.89282,65.77217),
                           base_delay = c(1.151e-01,7.456e-04,2.424e-03),
                           capital    = 100000,
                           teams      = 3,
                           fixed_cost = 250, 
                           cpr        = 5, # clients per round
                           gen_vector = matrix(runif(10*4),ncol = 4),
                           lambda     = 0.1,
                           s2p        = 0.005,
                           s2q        = 0.25){
  
  if (rounds <= 1)             stop("rounds must be bigger than one")
  if (companies <= 1)          stop("companies must be bigger than one")
  if (capital <= 1)            stop("capital must be bigger than one")
  if (length(base_costs) != 3) stop("base_costs must have three elements")
  if (length(base_delay) != 3) stop("base_delay must have three elements")

  # Create all companies
  market = companyGen(companies, base_costs, rounds, capital, teams)

  for (r in 1:rounds){
    
    for (k in 1:cpr){
      # Create a client with distance and volume
      client    = clientGen()
      # Based on client  and on their own set up, companies offer their prices
      prices    = companyPricing(client, market, companies, lambda, s2p, gen_vector)
      # client select the best company based on s/he preferences
      selection = clientSelect(client, prices, market)
      # Allocation of a team to a job
      if (selection > 0) market = teamsDispatchment(market, base_delay, client, 
                                                    selection, prices, s2q, gen_vector)
    }
    # Based client preferences, the companies status are changed
    market    = companyModStatus(market, client, selection, base_delay, 
                                 prices, r, fixed_cost, companies)
  }
  market
}

# generating a constumer with preferences and an order
clientGen <- function(){
  
  alpha    = runif(1)*.98 + 0.01
  distance = getDistance()
  volume   = getVolume()
  client   = list(alpha    = alpha, 
                  distance = distance,
                  volume   = volume)
  
}
clientSelect <- function(client, prices, market){
  
  # obs: the highest price goes to 1 and the others are normalized
  # Cobb-Douglas function gives the client utility
  # If alpha = 0 => client chooses the company with the best quality
  # If alpha = 1 => client chooses the company with the lower price
  quality = NULL
  # Quality
  for (i in 1:companies){
    eval(
      parse(
        text = paste0('quality = c(quality,mean(market$Q',i,'))')
      )
    )
  }
  
  
  if (sum(is.infinite(prices))){
    lst  = which(is.infinite(prices))
    if (length(prices[-lst]) != 0){
      maxp = max(prices[-lst])
      tmp  = ((6 - 5*prices/maxp)^client$alpha)*(quality^(1-client$alpha))
      tmp[lst] = 0
      selection = which.max(tmp)
    } else {
      selection = -1
    }
    
  } else {
    maxp = max(prices)
    tmp  = ((6 - 5*prices/maxp)^client$alpha)*(quality^(1-client$alpha))
    selection = which.max(tmp)
  }
  selection
}
# Generate a value for distance based on actual data
getDistance <- function(){
  n        = sample(nrow(data),1)
  distance = data$distance[n]*(1 + (runif(1)-0.5)/10)
}
# Generate a value for volume based on actual data
getVolume <- function(){
  n        = sample(nrow(data),1)
  volume   = data$volume[n]*(1 + (runif(1)-0.5)/10)
}

# client e avalia a empresa baseado nas características da empresa
clientEvaluation <- function(client, market, selection, s2q, gen_vector){
  
  quality = rtruncnorm(1, a=1,   b=5,  mean = (4*gen_vector[selection,1]+1), sd = sqrt(s2q*gen_vector[selection,2]))
  quality
}

# gera uma empresa com custos/capital inicial aleatórios e comportamento padrão
companyGen <- function(companies  = NULL, 
                       base_costs = NULL, 
                       rounds     = NULL,
                       capital    = NULL,
                       teams      = NULL){
  
  market = list()
  for (i in 1:companies){
    eval(
      parse(
        text = paste0('market = append(market,list(C',i,' = c(base_costs[1],base_costs[2],base_costs[3])))')
      )
    )
  }
  market = append(market,list(balance = as.data.frame(matrix(rep(NaN,companies*rounds),nrow = rounds))))
  market$balance[1,] = rep(capital,companies)
  if (length(teams) > 1){
    if (length(teams) != companies) stop("the size of teams must be equal to companies")
    market = append(market,list(teams = teams))
  } else if(teams < 1){
    market = append(market,list(teams = round(rep(3,companies)*(1 + (runif(companies)-0.5)))))
  } else {
    market = append(market,list(teams = rep(teams,companies)))
  }
  for (i in 1:companies){
    eval(
      parse(
        text = paste0('market = append(market,list(T',i,' = rep(0,market$teams[i])))')
      )
    )
  }
  market = append(market,list(total_teams = market$teams))
  market = append(market,list(market_share= NULL)) # History of choices from clients
  # Quality
  for (i in 1:companies){
    eval(
      parse(
        text = paste0('market = append(market,list(Q',i,' = 5))')
      )
    )
  }
  # Pending Quality
  for (i in 1:companies){
    eval(
      parse(
        text = paste0('market = append(market,list(PQ',i,' = rep(NaN,market$teams[i])))')
      )
    )
  }
  # Pending Bill
  for (i in 1:companies){
    eval(
      parse(
        text = paste0('market = append(market,list(PB',i,' = rep(NaN,market$teams[i])))')
      )
    )
  }
  market
}

# Pricing based on client distance and volume
companyPricing <- function(client, market, companies, lambda, s2p, gen_vector){
  
  prices = rep(Inf,companies)
  for (j in 1:companies){
    if (market$teams[j] > 0){
      P = rtruncnorm(1, a=-.2, b=.2, mean = 0.2*( (gen_vector[j,3]-0.5 + lambda*(gen_vector[j,1]-0.5) )/(0.5*(1+lambda))), sd = sqrt(s2p*gen_vector[j,4]) )
      eval(
        parse(
          text = paste0('prices[j] = (1 + P)*sum(market$C',j,'*c(1,client$distance, client$volume))')
        )
      )
    }
  }
  prices
  
}
teamsDispatchment <- function(market, base_delay, client, selection, prices, s2q, gen_vector){
  
  # Assign to the winner company the delivery
  delay   = getDelay(base_delay,client)
  quality = clientEvaluation(client, market, selection, s2q, gen_vector) # client  evaluate the company
  eval(
    parse(
      text = paste0('for (i in 1:length(market$T',selection,')){ if(market$T',selection,'[i] == 0){market$T',selection,'[i] = delay;market$PB',selection,'[i] = prices[selection];market$PQ',selection,'[i] = quality; market$teams[selection] = market$teams[selection] - 1; break } }')
    )
  )
  market$market_share = c(market$market_share,selection)
  market
}

# deduz diária das equipes e se a empresa conseguiu um contrato altera o número de equipes disponíveis. Caso a empresa tenha algum contrato finalizado, adiciona o valor ao saldo da empresa e verifica se a empresa ainda tem saldo para continuar no mercado.
companyModStatus <- function(market, client, selection, base_delay, prices, r, fixed_cost, companies){
  
  # If there is a team outside, deduct 1 day
  # If the team returned then add the bill value and the stars
  for (j in 1:companies){
    eval(
      parse(
        text = paste0('for (i in 1:length(market$T',j,')){ if(market$T',j,'[i] > 0){market$T',j,'[i] = market$T',j,'[i] -1; if (market$T',j,'[i] == 0){ market$Q',j,' = c(market$Q',j,',market$PQ',j,'[i]); market$PQ',j,'[i] = NaN; market$balance[r-1,j] = market$balance[r-1,j]+market$PB',j,'[i]; market$PB',j,'[i] = NaN; market$teams[j] = market$teams[j] + 1} } }')
      )
    )
  }
  
  
  for (j in 1:companies){
    if (r > 1) market$balance[r,j] = market$balance[r-1,j]-fixed_cost*market$total_teams[j]
    if (market$balance[r,j] <= 0){
      market$teams[j]     = 0
      market$balance[r,j] = 0
    }
  }
  market
}

getDelay <- function(base_delay = NULL,
                     client   = NULL){
  # make a dataframe with new data
  newdata = data.frame(distance = client$distance, volume = client$volume)
  # change the coefficients
  model$coefficients = base_delay
  # use 'predict()' to run model on new data
  delay = as.numeric(round(predict(model, newdata = newdata, type = "response")))
}
