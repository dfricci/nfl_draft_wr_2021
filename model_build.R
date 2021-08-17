#overwrite our final data to save it
load("/Users/danielricci/Documents/Football Analytics/Football Database/R Code/data.RData")
data_final <- data
data_final <- data[(data$Year <= 2020), ]
rownames(data_final) <- 1:nrow(data_final) 
data_21 <- data[(data$Year > 2020), ]
rownames(data_21) <- 1:nrow(data_21) 


data_final$Rnd <- as.factor(data_final$Rnd)

#set up x and y variables for testing
x_vars = c("speed_score", "explosion_score", "quickness_score", "strength_score", "catch_radius", "athletic_score", 
         "season_norm_rec_rec_game","season_norm_rec_yards_game", "season_norm_rec_tds_game", "season_norm_rec_avg_game",
         "career_norm_rec_rec_game", "career_norm_rec_yards_game", "career_norm_rec_tds_game", "career_norm_rec_avg_game",
         "conference_rating", "career_usage_rate", "season_usage_rate", "season_production_score", "career_production_score")

y_vars = "Rnd"

#variable selection
reg = lm(paste("Pick","~", paste(x_vars, collapse = "+"), sep=""), data = data_final, model = FALSE)
reg = data.frame(summary(reg)$coefficients)
reg$vars <- row.names(reg)

library(MASS)
set.seed(824)
split = sample(nrow(data_final), round(nrow(data_final) * 0.6, 0))

#training
prob_limit = c (0.1, 0.145, 0.15, 0.18, 0.19, 0.195, 0.2, 0.3, 0.5, 0.6, 0.7, 0.9, 1)
train_coefs = list()
for (i in 1:length(prob_limit)){
  vars = reg$vars[reg$Pr...t.. < prob_limit[i] ]
  vars = vars[2:length(vars)]
  formla = as.formula(paste("Rnd~", paste(vars, collapse="+"), sep=""))
  reg_temp = polr(formla, data = data_final[split, c("Rnd", vars)], Hess = F, model = F )
  
  temp = list()
  temp[[1]] = coef(reg_temp)
  temp[[2]] = reg_temp$zeta
  print(coef(reg_temp))
  print(reg_temp$zeta)
  
  train_coefs[[i]] = temp
  train_coefs = rbind(train_coefs, temp)
  rm(reg_temp, temp)
  print(i)
}


i = 9

#testing
results <- data.frame()
for (i in 1:length(prob_limit)) {
  coefs <- unlist(train_coefs[[i]][1])
  zeta <- unlist(train_coefs[[i]][2])
  
  model_vars <- as.matrix(data_final[-split, names(coefs)])
  
  pred_raw <- model_vars%*%coefs
  one <- 1/(1+exp(pred_raw-zeta[1]))
  two <- 1/(1+exp(pred_raw-zeta[2])) - one
  three <- 1/(1+exp(pred_raw-zeta[3])) - one- two
  four <- 1/(1+exp(pred_raw-zeta[4])) - one- two - three
  five <- 1/(1+exp(pred_raw-zeta[5])) - one- two - three - four
  six <- 1/(1+exp(pred_raw-zeta[6])) - one- two - three - four - five
  seven <- 1/(1+exp(pred_raw-zeta[7])) - one- two - three - four - five - six
  eight <- 1 - (1/(1+exp(pred_raw-zeta[7])))
  make_predict <- cbind (one, two, three, four, five, six, seven, eight)
  make_predict <- data.frame(make_predict)
  
  make_predict <- as.matrix(make_predict)
  avgLL <- mean(log(rowSums(make_predict*(model.matrix(as.formula("Player~ Rnd - 1"), data = data_final[-split, c("Rnd", "Player")])))))
  
  results <- rbind(results, 
                   cbind(avgLL, i))
}

#Choose best model 
i = results$i[abs(results$avgLL) == min(abs(results$avgLL))]

#run full model
vars = reg$vars[reg$Pr...t.. < prob_limit[i] ]
vars = vars[2:length(vars)]
formla = as.formula(paste("Rnd~", paste(vars, collapse = "+"), sep = ""))
data_final = data_final[ ,c("Rnd", vars)]
reg_temp = polr(formla, data = data_final[, c("Rnd", vars)], Hess = F, model= F)
final_coefs = coef(reg_temp)
final_zeta = reg_temp$zeta
print(final_coefs)
print(final_zeta)

#predict on new data
model_vars <- as.matrix(data_21[,names(final_coefs)])
pred_raw <- model_vars%*%final_coefs
one <- 1/(1+exp(pred_raw-final_zeta[1]))
two <- 1/(1+exp(pred_raw-final_zeta[2])) - one
three <- 1/(1+exp(pred_raw-final_zeta[3])) - one- two
four <- 1/(1+exp(pred_raw-final_zeta[4])) - one- two - three
five <- 1/(1+exp(pred_raw-final_zeta[5])) - one- two - three - four
six <- 1/(1+exp(pred_raw-final_zeta[6])) - one- two - three - four - five
seven <- 1/(1+exp(pred_raw-final_zeta[7])) - one- two - three - four - five - six
eight <- 1 - (1/(1+exp(pred_raw-final_zeta[7])))
make_predict <- cbind (one, two, three, four, five, six, seven, eight)
make_predict <- data.frame(make_predict)
make_predict <- as.matrix(make_predict)
result = data.frame(data_21$Player, data_21$School, data_21$Rnd, make_predict)

rm(one)
rm(two)
rm(three)
rm(four)
rm(five)
rm(six)
rm(seven)
rm(eight)
rm(make_predict)
rm(model_vars)
rm(pred_raw)
rm(reg)
rm(reg_temp)
rm(results)
rm(train_coefs)




