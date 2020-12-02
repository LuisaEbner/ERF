################################################################################
#                                                                              #
#         Predicting Diabetes: Experiments on the concept drift                #   
#                                                                              #
################################################################################

# Libraries
library(ggplot2)
library(plyr)
library(dplyr)


# External functions
source("./experiments/diabetes/erf_diabetes_dataprep.R")
source("./ERF/erf_main.R")

#===============================================================================
#                          EXPLORATORY ANALYSIS
#===============================================================================

data <- read.csv(file = './data sets/diabetes.csv', header = T)
data <- prepare_diabetes_data(data)
data$Age.greater.35 <- factor((as.numeric(data$Age > 35)), levels = c("0","1"))
data$overweight <- factor((as.numeric(data$BMI > 30)), levels = c("0","1"))

# 1. Overlayed histograms according to normal- and overweight population

# a) Pregnancies
mu_preg_w <- ddply(data, "overweight", summarise, grp.mean=mean(Pregnancies))
p1  <- ggplot(data, aes(x=Pregnancies, fill = overweight, color=overweight)) +
             geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 1)+
             geom_density(alpha=0.4)+
             geom_vline(data=mu_preg_w, aes(xintercept=grp.mean, color=overweight), linetype="dashed")+
             theme(legend.position="right") +
             scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
             scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
             labs(title="Pregnancies plot grouped by being overweight (BMI > 30)",x="Pregnancies", y = "Density")+
             theme(plot.title = element_text(size=15),
                          legend.text=element_text(size=11),
                          text = element_text(size = 14), 
                          axis.text = element_text(size = 12)) 
p1
pdf("Pregnancies_overweight.pdf")
p1
dev.off()


# b) Blood Pressure
mu_bp_w <- ddply(data, "overweight", summarise, grp.mean=mean(BP))
p1  <- ggplot(data, aes(x=BP, fill = overweight, color=overweight)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 5)+
  geom_density(alpha=0.4)+
  geom_vline(data=mu_bp_w, aes(xintercept=grp.mean, color=overweight), linetype="dashed")+
  theme(legend.position="right") +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  labs(title="Blood pressure plot grouped by being overweight (BMI > 30)",x="Blood pressure", y = "Density")+
  theme(plot.title = element_text(size=15),
        legend.text=element_text(size=11),
        text = element_text(size = 14), 
        axis.text = element_text(size = 12)) 
p1
pdf("BP_overweight.pdf")
p1
dev.off()


# a) Glucose
mu_glucose_w <- ddply(data, "overweight", summarise, grp.mean=mean(Glucose))
p1  <- ggplot(data, aes(x=Glucose, fill = overweight, color=overweight)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 5)+
  geom_density(alpha=0.4)+
  geom_vline(data=mu_glucose_w, aes(xintercept=grp.mean, color=overweight), linetype="dashed")+
  theme(legend.position="right") +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  labs(title="Glucose plot grouped by being overweight (BMI > 30)",x="Glucose level", y = "Density")+
  theme(plot.title = element_text(size=15),
        legend.text=element_text(size=11),
        text = element_text(size = 14), 
        axis.text = element_text(size = 12)) 
p1
pdf("Glucose_overweight.pdf")
p1
dev.off()


# b) Diabetes
mu_y_w <- ddply(data, "overweight", summarise, grp.mean=mean(as.numeric(y)))
p2 <-  ggplot(data, aes(x=as.numeric(y), fill = overweight, color=overweight)) +
             geom_histogram(aes(y=..density..), position="identity", alpha=0.4, binwidth = 1)+
             geom_vline(data=mu_y_w, aes(xintercept=grp.mean, color=overweight), linetype="dashed")+
             theme(legend.position="right") +
             scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
             scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
             theme(axis.text.x = element_blank()) +
             labs(title="Diabetes grouped by being overweight (BMI > 30)",x="Diabetes(1 = no, 2 = yes)", y = "Density")+
             theme(plot.title = element_text(size=15),
                   legend.text=element_text(size=11),
                   text = element_text(size = 14), 
                   axis.text = element_text(size = 12)) 
p2
pdf("y_overweight.pdf")
p2
dev.off()

#-------------------------------------------------------------------------------

# 2. Overlayed histograms according to Age (being "age.greater.35" (>35) or of advanced age)

# a) Pregnancies
mu_preg_a <- ddply(data, "Age.greater.35", summarise, grp.mean=mean(Pregnancies))
p3 <-ggplot(data, aes(x=Pregnancies, fill = Age.greater.35, color=Age.greater.35)) +
            geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 1)+
            geom_density(alpha=0.4)+
            geom_vline(data=mu_preg_a, aes(xintercept=grp.mean, color=Age.greater.35), linetype="dashed")+
            theme(legend.position="right") +
            scale_color_brewer(palette="Dark2")+
            scale_fill_brewer(palette="Dark2") +
            labs(title="Pregnancies plot grouped by Age (> 35)",x="Number of Pregnancies", y = "Density")+
            theme(plot.title = element_text(size=15),
                  legend.text=element_text(size=11),
                  text = element_text(size = 14), 
                  axis.text = element_text(size = 12)) 
p3
pdf("Pregnancies_age.pdf")
p3
dev.off()


# b) Glucose
mu_glucose_a <- ddply(data, "Age.greater.35", summarise, grp.mean=mean(Glucose))
p4  <- ggplot(data, aes(x=Glucose, fill = Age.greater.35, color=Age.greater.35)) +
              geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 5)+
              geom_density(alpha=0.4)+
              geom_vline(data=mu_glucose_a, aes(xintercept=grp.mean, color=Age.greater.35), linetype="dashed")+
              theme(legend.position="right") +
              scale_color_brewer(palette="Dark2")+
              scale_fill_brewer(palette="Dark2") +
              labs(title="Glucose plot grouped by Age (> 35)",x="Glucose level", y = "Density")+
              theme(plot.title = element_text(size=15),
                    legend.text=element_text(size=11),
                    text = element_text(size = 14), 
                    axis.text = element_text(size = 12)) 
              
p4
pdf("Glucose_age.pdf")
p4
dev.off()


# c) Blood Pressure
mu_bp_a <- ddply(data, "Age.greater.35", summarise, grp.mean=mean(BP))
p5 <-ggplot(data, aes(x=BP, fill = Age.greater.35, color=Age.greater.35)) +
            geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 5)+
            geom_density(alpha=0.4)+
            geom_vline(data=mu_bp_a, aes(xintercept=grp.mean, color=Age.greater.35), linetype="dashed")+
            theme(legend.position="right") +
            scale_color_brewer(palette="Dark2")+
            scale_fill_brewer(palette="Dark2")+
            labs(title="Blood pressure plot grouped by Age (> 35)",x="Blood Pressure", y = "Density")+
            theme(plot.title = element_text(size=15),
                  legend.text=element_text(size=11),
                  text = element_text(size = 14), 
                  axis.text = element_text(size = 12)) 

p5
pdf("BP_age.pdf")
p5
dev.off()

# d) Diabetes
mu_y_a <- ddply(data, "Age.greater.35", summarise, grp.mean=mean(as.numeric(y)))
p6 <-ggplot(data, aes(x=as.numeric(y), fill = Age.greater.35, color=Age.greater.35)) +
             geom_histogram(aes(y=..density..), position="identity", alpha=0.4, binwidth = 1)+
             geom_vline(data=mu_y_a, aes(xintercept=grp.mean, color=Age.greater.35), linetype="dashed")+
             theme(legend.position="right") +
             scale_color_brewer(palette="Dark2") +
             scale_fill_brewer(palette="Dark2") +
             labs(title="Histogram plot grouped by Age (> 35)",x="Diabetes (1 = no, 2 = yes)", y = "Density")+
             theme(plot.title = element_text(size=15),
                    legend.text=element_text(size=11),
                    text = element_text(size = 14), 
                    axis.text = element_text(size = 12)) 
p6
pdf("y_age.pdf")
p6
dev.off()

