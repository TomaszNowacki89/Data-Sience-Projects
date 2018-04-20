getwd()
# path change
setwd("C:/Users/tomas/OneDrive/Dokumenty/game model")

# loading files
participants = read.csv("participants.csv", sep = ';')
performance = read.csv("performance.csv", sep = ';')

# heads of the datasets
head(participants)
head(performance)


# merging the datasets together
game = merge(participants, performance, by.x = 'id', by.y = 'id' )
head(game)

# utilizing the dplyr library
library(dplyr)


# reorginising the dataset

# the points and values for each player are in rows, but they should be in columns
game = game %>% arrange(id, round)
predict_game = reshape(game, idvar = 'id', timevar = 'round', direction = "wide")
predict_game = as.data.frame(predict_game)
head(predict_game)
# now it is necessary to get rid of some columns
class(predict_table)
drop = c("team.1","team.2","team.3","team.4","team.5","team.6","team.7","team.8",
                                 "team.9","team.10","team.11","team.12","team.13","team.14","team.15","team.16",
                                 "team.17","team.18","team.19","team.20","type.1","type.2","type.3","type.4",
                                 "type.5","type.6","type.7","type.8","type.9","type.10","type.11","type.12",
                                 "type.13","type.14","type.15","type.16","type.17","type.18","type.19","type.20")
predict_game = predict_game[, !(names(predict_game) %in% drop)]
head(predict_game)

# the variable "team.0" and "id" is not necessary - it is also erased

predict_game = predict_game[, -c(1,2)]
head(predict_game)

# checking the lack of data in the dataset

library(Amelia)
missmap(predict_game, legend = TRUE)

# for using the neural net for prediction we need the rows with full data

# deleting the rows with the lack of data
library(tidyr)
predict_game_clean = predict_game %>% drop_na()

# the structure of the final data.frame
str(predict_game_clean)

# 
library(nnet)
library(caret)
# Normalization

preProcValues <- preProcess(predict_game_clean, method = c("range"))
predict_game_nn <- predict(preProcValues, predict_game_clean)
str(predict_game_nn)

#cleaning the irrelevant data
rm(game, participants, performance, predict_game, predict_game_clean, preProcValues, drop)

# empty vector for the accuracy in each interation of the model
correct_ratio_test_nn_100 = c()

# Formula

formula_1 = value.20 ~ .


# The split into the training set and test set. It is done in the loop for gathering the
# multiple results from the model performance, with different training set and test set

for (i in 1:100)
{
# indexing rows
number_rows = dim(predict_game_nn)[1]
index = sample(number_rows, round(0.7*number_rows))
training_set_nn = predict_game_nn[index,]
test_set_nn = predict_game_nn[-index,]

# neural net training

model_nn = nnet(formula = formula_1, data = training_set_nn, size = 3, decay = 4e-4,
                linout = TRUE)

# test sieci neuronowej 
nn.results <- predict(model_nn, test_set_nn[,1:42])

table_res_nn = cbind(nn.results, test_set_nn[,43])
table_res_nn = as.data.frame(table_res_nn)

# minus values as 0

table_res_nn$V1[table_res_nn$V1 < 0] = 0

# Scailing by 30 (max - min), creating two new variables, rounding the results of the model to integers
table_res_nn$predicted = table_res_nn$V1*30
table_res_nn$actual = table_res_nn$V2*30
table_res_nn$predicted = round(table_res_nn$predicted, digits = 0)

# Assigning the correct predictions as 1 and false by 0
table_res_nn$difference = table_res_nn$actual - table_res_nn$predicted
table_res_nn$difference[table_res_nn$difference == 0] = 1
table_res_nn$difference[table_res_nn$difference != 1] = 0
table_res_nn

#table_res_test
correct_ratio_test_nn = sum(table_res_nn$difference)/length(table_res_nn$difference)
correct_ratio_test_nn_100[i] = correct_ratio_test_nn
}

# Podsumowanie predykcji modelu w 100 iteracjach:

# Wektor procentowej poprawnosci predykcji w kazdej iteracji:
correct_ratio_test_nn_100
# Sredni wynik modelu
mean_val_nn = mean(correct_ratio_test_nn_100)
# Odchylenie standardowe
sd_val_nn = sd(correct_ratio_test_nn_100)
# Wyniki
mean_val_nn
sd_val_nn

# Conclusion:
# The neural net predicts with very good accuracy the results in the last 20th round of the game
# After looping the script one houndred times the mean value of the accuracy (mean_val_nn) is around
# 98.7% with the standard deviation of 0.018%. The neural net uses 3 neurons in hidden layer.



