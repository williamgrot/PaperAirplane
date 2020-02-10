library(readr)
paper <- read_csv("paper.csv")

## find out all the squared combination of variables in the model
model <- lm(sec~(W1_91+W1_89+W1_down+W2_1cm+W2_2cm+
              W3_tail+W4_ha_air+W5_45degree+W5_90degree+W6_tape+W7_clip)^2,data=paper)
model <- step(model,k=log(nrow(paper))) 

## check the lowest BIC value
BIC(model)

## model1 is the baisc model without the combination
model1 <- lm(sec~W1_91+W1_89+W1_down+W2_1cm+W2_2cm+
              W3_tail+W4_ha_air+W5_45degree+W5_90degree+W6_tape+W7_clip,data=paper)
summary(model1)

## model2 is the refined by BIC model with combination variates
model2 <- lm(sec ~ W1_91 + W1_89 + W1_down + W2_1cm + W2_2cm + W3_tail + W4_ha_air + 
               W5_45degree + W5_90degree + W6_tape + W7_clip + W1_91:W2_1cm + 
               W1_91:W3_tail + W1_89:W2_1cm + W1_89:W2_2cm,data=paper)
summary(model2)
