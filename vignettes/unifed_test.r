library(data.table)
library(unifed)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


car.data <- read.csv("car.csv")

car.data$area <- as.character(car.data$area) 
car.data$area <- ifelse(car.data$area %in% c("A","B","C","D","E"),"ABCDE",car.data$area) 
car.data$agecat <- ifelse(car.data$agecat %in% c(1,2),"12",paste(car.data$agecat))
    
car.data$agecat <- factor(car.data$agecat)
car.data$veh_age <- factor(car.data$veh_age)

agg.data <- aggregate(cbind(exposure,rep(1, dim(car.data)[1])) ~ gender + agecat + area +veh_age,
                      data=car.data,
                      FUN=sum)

colnames(agg.data)[colnames(agg.data) == "V2"] <- "weight"
colnames(agg.car.data)[colnames(agg.car.data) == "exposure"] <- "class.exposure"

X <- model.matrix( ~ gender + agecat + area + veh_age, car.data )

model.data <- list(M=dim(X)[1],
                       P=dim(X)[2],
                       X=X,
                       y= car.data$class.exposure,
                       ws= car.data$weight)




model.list <- stanc_builder("unifed_example.stan",
                            isystem=unifed.stan.folder())


stan.model <- stan(model_code=model.list$model_code,
                   data=model.data,
                   warmup=1e4,
                   iter=3e4)
