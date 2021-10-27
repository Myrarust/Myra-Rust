# FINAL PROJECT 

# Name: RUST, MYRA

# Date: 14 MAY 2020

library("ggplot2")
library("readxl")
library("dplyr")
library("broom")
library("reshape2")
library("scales")

path <- "C:\\Users\\myraw\\RStudio\\DSC520Spring2020\\project_dataset.xlsx"
ivf <- read_excel(path, sheet = 1, col_names = TRUE, col_types = NULL)
str(ivf)

#reducing columns by subsetting data set
ivf_sub <- ivf[, c(1,3,5:16,19:29,33:35,37,38,44:48,58:60,68,69)]

#Renaming columns
names(ivf_sub) <- c("age", "totalCycles", "ivfCycles", "donorCycles", "totalPreg",
                    "ivfPreg", "donorPreg", "totalLivebirth", "ivfLivebirth", "donorLivebirth", 
                    "femaleInfPrime", "femaleInfSec", "maleInfPrime", "maleInfsec",
                    "tubalDisease", "ovulatoryDisorder", "maleFactors", "unexplained", 
                    "endometriosis", "cervicalFactors", "femaleFactors", "spermConcentration", 
                    "spermMorphology", "spermMotility", "spermImmunoFactors", "eggDonorAge", 
                    "spermDonorAge", "donorEmb", "treatmentType", "specificTreatment", 
                    "eggSource", "spermSource", "freshEmb", "frozenEmb", "eggsThawed", 
                    "numEmbXfer", "micInjeggEmbXfer", "storedEmb", "liveBirth", 
                    "numLiveBirths")

#Rearrange order of columns and remove a few more
ivf2 <- ivf_sub[,c(1, 2, 5, 8, 3, 6, 9, 4, 7, 10:27, 29:32, 28, 33:40)]
ivf3 <- ivf2[ -c(27, 31, 37, 38) ]
ivf3 <- ivf3[ -c(33)]
ivf3 <- ivf3[ -c(34)]

#Change input of >=5 to a useable value
ivf3[ivf3 == ">=5"] <- "6"

#Change charater variable into factor
ivf3$age <- as.factor(ivf3$age) 
ivf3$treatmentType <- as.factor(ivf3$treatmentType)
ivf3$specificTreatment <- as.factor(ivf3$specificTreatment)
ivf3$eggSource <- as.factor(ivf3$eggSource)
ivf3$eggDonorAge <- as.factor(ivf3$eggDonorAge)

#Change character variable into numberic variable
ivf3$donorPreg <- as.numeric(as.character(ivf3$donorPreg))
ivf3$numEmbXfer <- as.numeric(as.character(ivf3$numEmbXfer))
ivf3$totalPreg <- as.numeric(as.character(ivf3$totalPreg))
ivf3$totalCycles <- as.numeric(as.character(ivf3$totalCycles))
ivf3$totalLivebirth <- as.numeric(as.character(ivf3$totalLivebirth))
ivf3$ivfCycles <- as.numeric(as.character(ivf3$ivfCycles))
ivf3$ivfPreg <- as.numeric(as.character(ivf3$ivfPreg))
ivf3$ivfLivebirth <- as.numeric(as.character(ivf3$ivfLivebirth))
ivf3$donorCycles <- as.numeric(as.character(ivf3$donorCycles))
ivf3$donorLivebirth <- as.numeric(as.character(ivf3$donorLivebirth))

#Section 2 Images
str(ivf3)
head(ivf3)
summary(ivf3)
glimpse(ivf3)

#RESEARCH Q1: Success rate of ivf vs other procedures
# Relevel specificTreatment to make it more manageble
levels(ivf3$specificTreatment)
levels(ivf3$specificTreatment) <- c("FER", "Generic DI", "ICI", "ICSI", "ICSI", 
                                    "ICSI", "ICSI", "ICSI", "ICSI", "IUI", "IVF", "IVF", 
                                    "IVF", "IVF", "IVF", "IVF", "IVF", "IVI", "UNK")

#data clean up
ivfq1 <- ivf3 %>% filter(totalCycles > 0)

#Q1 Test Train Split
N1 <- nrow(ivfq1)
target1 <- round(.75 * N1)
gp1 <- runif(N1)
q1_train <- ivfq1[gp1 < .75,]
q1_test <- ivfq1[gp1 >= .75,]

#Create model
q1_model <- lm(totalPreg ~ totalCycles + specificTreatment, data = q1_train)
q1.2_model <- lm(totalPreg ~ totalCycles, data = q1_train)
summary(q1_model)
augment(q1_model)
anova(q1_model)

#Filter unneeded columns for plotting
q1_train <- q1_train %>% filter(specificTreatment != "FER", specificTreatment != "UNK")

ggplot(q1_train, aes(x = totalCycles, y = totalPreg)) + 
    geom_point(position = "jitter", alpha = 0.1, na.rm = TRUE) +
    geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
    facet_wrap(~specificTreatment, ncol = 2) + 
    ggtitle("Number of Pregnancies by Treatment Cycles & Treatment Type") + 
    xlab("Number of Treatment Cycles") + 
    ylab("Number of Pregnancies")
    

#RESEARCH Q2: Success rate of IVF by age
#Q2 data clean up
ivfq2 <- ivf3 %>% filter(ivfCycles > 0, age != 999)
ivfq2$ivfSuccessRate <- ivfq2$ivfPreg / ivfq2$ivfCycles

#Q2 Test Train Split
N2 <- nrow(ivfq2)
target2 <- round(.75 * N2)
gp2 <- runif(N2)
q2_train <- ivfq2[gp2 < .75,]
q2_test <- ivfq2[gp2 >= .75,]

#Plot of Number of IVF Pregnancies by Age
ggplot(q2_train, aes(x = age, y = ivfPreg)) + 
    geom_jitter(width = 0.4, height = 0.4, alpha = 0.05) +
    ggtitle("IVF Pregnancies by Age") +
    xlab("Age Group") + 
    ylab("Total IVF Pregnancies")

#Calculate Mean of ivfSuccessRate by age
successByAge <- aggregate(q2_train[, "ivfSuccessRate"], list(q2_train$age), mean)
names(successByAge)[1] <- "age"
names(successByAge)[2] <- "meanIVFSuccessRate"
successByAge

#Plot mean of ivfSuccessRate by age
ggplot(successByAge, aes(x = age, y = meanIVFSuccessRate)) + 
    geom_point() +
    ggtitle("Mean IVF Success Rate by Age Group") +
    xlab("Age Group") + 
    ylab("Average IVF Success Rate (Percentage)")

#Calculate Mean of live births by age
liveBirthsByAge <- aggregate(q2_train[, "numLiveBirths"], list(q2_train$age), mean)
names(liveBirthsByAge)[1] <- "age"
names(liveBirthsByAge)[2] <- "meanLiveBirths"
liveBirthsByAge

#Plot mean of ilive births by age
ggplot(liveBirthsByAge, aes(x = age, y = meanLiveBirths)) + 
    geom_point() +
    ggtitle("Mean IVF Live Birth Rate by Age Group") +
    xlab("Age Group") + 
    ylab("Average IVF Live Birth Rate (Percentage)")


#RESEARCH Q3: Largest positive & negative effects on success
#create data set with variables to compare
ivfq3 <- ivfq2[,c(6, 15:20, 22:24, 30:32, 35)]
ivfq3[ivfq3==""] <- NA
ivfq3 <- na.omit(ivfq3)

#Q3 Test Train Split
N3 <- nrow(ivfq3)
target3 <- round(.75 * N3)
gp3 <- runif(N3)
q3_train <- ivfq3[gp3 < .75,]
q3_test <- ivfq3[gp3 >= .75,]

#determine correlation between ivfSuccessRate and infertility reasons
cor(q3_train$ivfSuccessRate, q3_train[, c(2:10)], use = "complete.obs")
factors_cor <- cor(q3_train[, c(2:10, 14)], use = "complete.obs")
factors_melt <- melt(factors_cor, varnames = c("x", "y"), value.name = "Correlation")
factors_melt <- factors_melt[order(factors_melt$Correlation), ]

ggplot(factors_melt, aes(x = x, y = y)) +
    geom_tile(aes(fill = Correlation)) +
    scale_fill_gradient2(low = muted("red"), mid = "white", high = "steelblue", 
                         guide = guide_colorbar(ticks = FALSE, barheight = 10)) +
    labs(x = NULL, y = NULL) + 
    ggtitle("IVF Success Rate and Infertility Type Correlation") + 
    

#determine correlation between ivfSuccessRate and type of embryo used
cor(q3_train$ivfSuccessRate, q3_train[, c(11:13)], use = "complete.obs")
factors_cor2 <- cor(q3_train[, c(11:14)], use = "complete.obs")
factors_melt2 <- melt(factors_cor2, varnames = c("x", "y"), value.name = "Correlation")
factors_melt2 <- factors_melt2[order(factors_melt2$Correlation), ]

ggplot(factors_melt2, aes(x = x, y = y)) +
    geom_tile(aes(fill = Correlation)) +
    scale_fill_gradient2(low = muted("red"), mid = "white", high = "steelblue", 
                         guide = guide_colorbar(ticks = TRUE, barheight = 10,)) +
    labs(x = NULL, y = NULL) + 
    ggtitle("IVF Success Rate and Embryo Type Correlation")


#RESEARCH Q4: Mulitple embryo increasing IVF success rate
str(ivfq2)
ivfq4 <- ivfq2[,c(5:7, 33:35)]
ivfq4[ivfq4==""] <- NA
ivfq4 <- na.omit(ivfq4)
ivfq4 <- ivfq4 %>% filter(numEmbXfer > 0)

#Q4 Test Train Split
N4 <- nrow(ivfq4)
target4 <- round(.75 * N4)
gp4 <- runif(N4)
q4_train <- ivfq4[gp4 < .75,]
q4_test <- ivfq4[gp4 >= .75,]

#create model for test
q4_model <- glm(ivfSuccessRate ~ numEmbXfer, data = q4_train, family = "poisson")
summary(q4_model)
augment(q4_model)

ggplot(q4_train, aes(x = numEmbXfer, y = ivfSuccessRate)) + 
    geom_jitter(width = 0.3, height = 0.3, alpha = 0.05) +
    geom_smooth(method = "glm", method.args = list(family = "poisson")) +
    ggtitle("IVF Success Rate by Number Embryos Transferred") + 
    xlab("Number or Embryos Transferred") + 
    ylab("IVF Success Rate (Percentage)")
    
#Calculate Mean of ivfSuccessRate by age
successByNumEmb <- aggregate(q4_train[, "ivfSuccessRate"], list(q4_train$numEmbXfer), mean)
names(successByNumEmb)[1] <- "numEmbXfer"
names(successByNumEmb)[2] <- "meanIVFSuccessRate"
successByNumEmb


#RESEARCH Q5: Mulitple embryo increasing Multiple Births
ggplot(q4_train, aes(x = numEmbXfer, y = numLiveBirths)) + 
    geom_jitter(width = 0.3, height = 0.3, alpha = 0.05) + 
    ggtitle("Live Births by Embryos Transferred") + 
    xlab("Number of Embryos Transferred") +
    ylab("Number of Live Births")

q5_model <- glm(numLiveBirths ~ numEmbXfer, data = q4_train, family = "poisson")
summary(q5_model)

#compare percentage rates for multiples by number of embryos
emb1 <- q4_train %>% filter(numEmbXfer == 1)
emb1_multiples <- sum(emb1$numLiveBirths > 1) / sum(emb1$numLiveBirths)
emb1_multiples

emb2 <- q4_train %>% filter(numEmbXfer == 2)
emb2_multiples <- sum(emb2$numLiveBirths > 1) / sum(emb2$numLiveBirths)
emb2_multiples

emb3 <- q4_train %>% filter(numEmbXfer == 3)
emb3_multiples <- sum(emb3$numLiveBirths > 1) / sum(emb3$numLiveBirths)
emb3_multiples





