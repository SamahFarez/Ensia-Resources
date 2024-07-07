# M/M/1
queing_system <- function(lambda, mu, total_time) {
  simulated_time <- 0  
  arrival_time <- rexp(1, lambda)  
  service_time <- Inf  
  queue <- 0  
  
  
  events <- data.frame(Type = character(), Temps = numeric(), queue_tmp = numeric())
  
  
  while (simulated_time < total_time) {
    if (arrival_time < service_time) {
      
      simulated_time <- arrival_time
      arrival_time <- simulated_time + rexp(1, lambda)
      
  
      if (service_time == Inf) {
        service_time <- simulated_time + rexp(1, mu)
      } else {
        queue <- queue + 1
      }
      
      events <- rbind(events, data.frame(Type = "arrival", Temps = simulated_time, queue_tmp = queue))
    } else {
      simulated_time <- service_time
      service_time <- if (queue > 0) simulated_time + rexp(1, mu) else Inf
      queue <- max(0, queue - 1)
      
      events <- rbind(events, data.frame(Type = "departure", Temps = simulated_time, queue_tmp = queue))
    }
  }
  
  return(events)
}



lambda <- 0.5
mu <- 1
total_time <- 1000  


simu <- queing_system(lambda, mu, total_time)


print(simu)

plot(simu$Temps, simu$queue_tmp, type = "s", col = ifelse(simu$Type == "arrival", "blue", "red"), xlab = "Time", ylab = "Queue Size", main = "M/M/1 Queue System")



M<-c()
for (i in 1:1000){
  x=rnorm(i,5,1)
  M[i]=mean(x)
}

plot (M,type='l',col='blue')
abline(h=5,col='red')

