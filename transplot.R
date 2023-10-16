# rm(list = ls())

setwd("~/LitReview")

library(ggplot2)
library(dplyr)
library(tidyverse)
library(janitor)
library(RColorBrewer)

# set theme for plots

Ctheme = theme(
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 14),
  axis.text.y = element_text(size = 12),
  plot.title = element_text(size = 18),
  axis.line = element_line(colour = "black", size = 1, linetype = 'solid'),
  panel.background = element_rect(fill = "white"),
  panel.grid.major.y = element_line(size = 0.5, linetype = 'dotted', colour = "light grey"),
  panel.grid.minor.y = element_line(size = 0.5, linetype = 'dotted', colour = "light grey"),
  panel.grid.major.x = element_blank(),
  panel.border = element_blank())

min(reviewdata$ï..Year)
max(reviewdata$ï..Year)

# import data

reviewdata <- read.csv("ReviewData.csv")
head(reviewdata) # check data
typeof(reviewdata) # check data type

notrans <- count(reviewdata, ScientificName) # number of translocation records per species
notrans.sort <- notrans[order(notrans$n, decreasing=TRUE), ] # sort number of translocations in decreasing order

### build plot for number of translocations for each species

transloc.plot <- ggplot(notrans.sort, aes(x = reorder(ScientificName, -n), y = n)) + 
  xlab("species") + 
  ylab("number of translocations") +
  geom_bar(stat="identity", colour = "black", fill = "black") + 
  scale_y_continuous(limits = c(0, 60), breaks = seq(0,60, by = 10), expand = c(0, 0)) +
  Ctheme + 
  theme(axis.text.x = element_text(face="italic", angle = 90, hjust = 1) #adjusts x axis text to italic and 90 degrees
  )
transloc.plot

notrans.sort$species.prop <- notrans.sort$n/sum(notrans.sort$n)


### build histogram for translocation site area (ha)

arealog10 <- log10(reviewdata$area_ha)
head(arealog10)
log10df <- as.data.frame(arealog10)
head(log10df)

10^(median(arealog10, na.rm=T)) # check median

area.plot <- ggplot(data = log10df, aes(arealog10)) +
  geom_histogram(colour = "white", fill = "black", bins = 25) + 
  labs(x = substitute(paste("area of translocation site (",log[10], " ha)", sep=""))) +
  ylab("number of translocations") +
  scale_x_continuous(breaks = seq(0,7, by = 1), expand = c(0, 0)) + #  removes gap between plot and axis
  scale_y_continuous(limits = c(0, 14), breaks = seq(0,14, by = 2), expand = c(0, 0)) +
  Ctheme
area.plot

### proportion of na in each column: area, source, # released, sex of individuals released, 
### year reviewed, survival, outcome, monitoring duration.

area.freq.na <- as.data.frame(table(reviewdata$area_ha, useNA = "ifany")/length(reviewdata$area_ha))
area.freq.na

source.freq.na <- as.data.frame(table(reviewdata$source_loc, useNA = "ifany")/length(reviewdata$source_loc))
source.freq.na

norelease.freq.na <- as.data.frame(table(reviewdata$total_released, useNA = "ifany")/length(reviewdata$total_released))
norelease.freq.na

mrelease.freq.na <- as.data.frame(table(reviewdata$M, useNA = "ifany")/length(reviewdata$M)) 
mrelease.freq.na

yearreview.freq.na <- as.data.frame(table(reviewdata$year_sampled, useNA = "ifany")/length(reviewdata$year_sampled)) 
yearreview.freq.na

survival.freq.na <- as.data.frame(table(reviewdata$survivalyears, useNA = "ifany")/length(reviewdata$survivalyears)) 
survival.freq.na

outcome.freq.na <- as.data.frame(table(reviewdata$outcome, useNA = "ifany")/length(reviewdata$outcome)) 
outcome.freq.na

monitoring.freq.na <- as.data.frame(table(reviewdata$monitoring_years, useNA = "ifany")/length(reviewdata$monitoring_years)) 
monitoring.freq.na

# rename the columns of the dataframes

names(area.freq.na)[2] <- "area"
names(source.freq.na)[2] <- "source"
names(norelease.freq.na)[2] <- "# released"
names(mrelease.freq.na)[2] <- "sex released"
names(yearreview.freq.na)[2] <- "year reviewed"
names(survival.freq.na)[2] <- "survival"
names(outcome.freq.na)[2] <- "outcome"
names(monitoring.freq.na)[2] <- "monitoring duration"

# combine the dataframes

freq.all <- merge(merge(merge(merge(merge(merge(merge(
  area.freq.na, 
  source.freq.na, by = "Var1", all = TRUE),
  norelease.freq.na, by = "Var1", all = TRUE),
  mrelease.freq.na, by = "Var1", all = TRUE),
  yearreview.freq.na, by = "Var1", all = TRUE),
  survival.freq.na, by = "Var1", all = TRUE),
  outcome.freq.na, by = "Var1", all = TRUE),
  monitoring.freq.na, by = "Var1", all = TRUE)

# extract only the NA frequencies

na.freq.all <- freq.all[freq.all$Var1 %in% c(NA),]
names(na.freq.all)[1] <- "variable"
na.freq.all
head(na.freq.all)
rownames(na.freq.all) <- na.freq.all$"275"
na.freq.all$"275" <- NULL
na.freq.all$"variable" <- NULL
head(na.freq.all)

# transpose to columns

na.freq.transpose <- as.data.frame(t(as.matrix(na.freq.all))) # t transposes
na.freq.transpose

# change row names to variable

has_rownames(na.freq.transpose)
na.freq.2 <- rownames_to_column(na.freq.transpose, var = "variable")
na.freq.2
names(na.freq.2)[2] <- "na.frequency"
na.freq.2

# calculate new column with proportion of data reported
na.freq.2$propreported <- 1-na.freq.2[,2]

# plot proportion of translocations that had data reported

na.freq.plot <- ggplot(na.freq.2, aes(x = reorder(variable, -propreported), y = propreported)) + 
  xlab(NULL) + 
  ylab("proportion of translocations with recorded information") +
  geom_bar(stat="identity", colour = "black", fill = "black") + 
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0,1.0, by = 0.2), expand = c(0, 0)) +
  Ctheme 
na.freq.plot

### plot outcomes of translocations

names(outcome.freq.na) <- c("outcome", "frequency")

outcomelabels1<- c("success", "fail", "pending", "not reported")

outcome.freq.plot <- ggplot(outcome.freq.na, aes(x = reorder(outcome, +frequency), y = frequency)) + 
  xlab("reported outcome") + 
  ylab("proportion of translocations") +
  geom_bar(stat="identity", colour = "black", fill = "black") + 
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0,0.5, by = 0.1), expand = c(0, 0)) +
  scale_x_discrete(labels = outcomelabels1) +
  Ctheme 
outcome.freq.plot

### how many of each species are in each conservation category?

### IUCN

head(reviewdata)
iucn.all <- as.data.frame(tabyl(reviewdata, ScientificName, IUCN))
typeof(iucn.all)

# build the columns to make the new dataframe
iucn.class <- c("not listed", "LC", "NT", "VU", "EN", "CR")
no.sp.iucn <- c(sum(iucn.all$'notlisted'>0), sum(iucn.all$LC>0), 
                sum(iucn.all$NT>0), sum(iucn.all$VU>0), sum(iucn.all$EN>0), sum(iucn.all$CR>0))
# join columns together
iucn.summary <- data.frame(iucn.class, no.sp.iucn)

# plot for number of species in each iucn category

sp.iucn.plot <- ggplot(iucn.summary, aes(x = iucn.class, y = no.sp.iucn), fill = iucn.class) + 
  xlab("IUCN classification") + 
  ylab("number of species translocated") +
  geom_bar(stat="identity", colour = "black", fill = "black") + 
  scale_y_continuous(limits = c(0, 15), breaks = seq(0,15, by = 2),expand = c(0, 0)) +
  scale_x_discrete(limits=iucn.summary$iucn.class) +
  Ctheme
sp.iucn.plot

### EPBC1999Listing

head(reviewdata)
epbc.all <- as.data.frame(tabyl(reviewdata, ScientificName, EPBC1999Listing))
typeof(epbc.all)

# build the columns to make the new dataframe
epbc.class <- c("not listed", "VU","EN","CE")
no.sp.epbc <- c(sum(epbc.all$'not listed'>0), sum(epbc.all$Vulnerable>0), sum(epbc.all$Endangered>0), sum(epbc.all$`Critically Endangered`>0))

# join columns together
epbc.summary <- data.frame(epbc.class, no.sp.epbc)

# plot for number of species in each epbc1999 category

sp.epbc.plot <- ggplot(epbc.summary, aes(x = epbc.class, y = no.sp.epbc), fill = epbc.class) + 
  xlab("EPBC 1999 classification") + 
  ylab("number of species translocated") +
  geom_bar(stat="identity", colour = "black", fill = "black") + 
  scale_y_continuous(limits = c(0, 18), breaks = seq(0,18, by = 2),expand = c(0, 0)) +
  scale_x_discrete(limits=epbc.summary$epbc.class) +
  Ctheme
sp.epbc.plot

### outcome x species

reviewdata$total_released <- as.numeric(reviewdata$total_released)

outcomelabels2 <- c("fail", "pending", "success", "not reported")

sp.outcome.plot <- ggplot(reviewdata, aes(x = outcome, y = total_released)) +
  geom_boxplot(colour = "black", fill = "white") +
  xlab("reported outcome") + 
  labs(y = substitute(paste("# individuals released ( ",log[10], ")", sep=""))) +
  scale_y_continuous(trans="log10") + 
  scale_x_discrete(labels = outcomelabels2) +
  Ctheme
sp.outcome.plot

median(reviewdata$total_released, na.rm = TRUE)

### survival years x total released ######### DON'T USE THIS 

head(reviewdata)

min(reviewdata$total_released, na.rm = TRUE)
max(reviewdata$total_released, na.rm = TRUE)

surv.release.plot <- ggplot(reviewdata, aes(x = total_released, y = survivalyears)) + 
  xlab("# of individuals released") + 
  ylab("survival (years)") +
  geom_point(colour = "black", fill = "black") + 
  scale_x_continuous(limits = c(0,85)) +
  Ctheme
surv.release.plot

### number of translocations per decade ### this will not auto update.

decaderelease <- c("<1960", "1960-1969", "1970-1979", "1980-1989", "1990-1999","2000-2009", ">2009")
freqrelease <- c(13, 12, 25, 25, 98, 68, 5)

decadereleasefreq <- data.frame(decaderelease, freqrelease)

decaderelease.plot <- ggplot(decadereleasefreq, aes(x = decaderelease, y = freqrelease), fill = decaderelease) + 
  ylab("number of translocations") +
  xlab(NULL) +
  geom_bar(stat="identity", colour = "black", fill = "black") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,100, by = 10),expand = c(0, 0)) +
  scale_x_discrete(limits=decadereleasefreq$decaderelease) +
  Ctheme
decaderelease.plot


#################################################################################
### translocation fail/success 
#################################################################################

### Logistic GLM weighted by integer multiples of longevity. 
### very weak evidence of a very weak effect of # released
## source functions & libraries
source("~/Statistics/new_lmer_AIC_tables3.R")
source("~/Statistics/r.squared.R")

library(lme4)
library(boot)
library(Hmisc)
library(ggplot2)

# functions
# Set functions
AICc <- function(...) {
  models <- list(...)
  num.mod <- length(models)
  AICcs <- numeric(num.mod)
  ns <- numeric(num.mod)
  ks <- numeric(num.mod)
  AICc.vec <- rep(0,num.mod)
  for (i in 1:num.mod) {
    if (length(models[[i]]$df.residual) == 0) n <- models[[i]]$dims$N else n <- length(models[[i]]$residuals)
    if (length(models[[i]]$df.residual) == 0) k <- sum(models[[i]]$dims$ncol) else k <- (length(models[[i]]$coeff))+1
    AICcs[i] <- (-2*logLik(models[[i]])) + ((2*k*n)/(n-k-1))
    ns[i] <- n
    ks[i] <- k
    AICc.vec[i] <- AICcs[i]
  }
  return(AICc.vec)
}
delta.AIC <- function(x) x - min(x) ## where x is a vector of AIC
weight.AIC <- function(x) (exp(-0.5*x))/sum(exp(-0.5*x)) ## Where x is a vector of dAIC
ch.dev <- function(x) ((( as.numeric(x$null.deviance) - as.numeric(x$deviance) )/ as.numeric(x$null.deviance))*100) ## % change in deviance, where x is glm object
linreg.ER <- function(x,y) { # where x and y are vectors of the same length; calls AICc, delta.AIC, weight.AIC functions
  fit.full <- lm(y ~ x); fit.null <- lm(y ~ 1)
  AIC.vec <- c(AICc(fit.full),AICc(fit.null))
  dAIC.vec <- delta.AIC(AIC.vec); wAIC.vec <- weight.AIC(dAIC.vec)
  ER <- wAIC.vec[1]/wAIC.vec[2]
  r.sq.adj <- as.numeric(summary(fit.full)[9])
  return(c(ER,r.sq.adj))
}
# import data

dat.full <- read.table("survival1.csv", header=T, sep=",")
dat.restr <- read.csv("survival2.csv")
dat <- dat.restr

colnames(dat) <- c("year_released", "species", "max_longevity", "total_released", "year_last_seen", "persist")

# yrs population persisted
dat$yearsPers <- dat$year_last_seen - dat$year_released + 1

# multiples of longevity
dat$longevMult <- ceiling(dat$yearsPers/dat$max_longevity)
dat

# plot data
dat0 <- subset(dat, persist==0)
dat1 <- subset(dat, persist==1)
mean(dat0$longevMult)
quantile(dat0$longevMult, probs=0.975)
quantile(dat0$longevMult, probs=0.025)
mean(dat1$longevMult)
quantile(dat1$longevMult, probs=0.975)
quantile(dat1$longevMult, probs=0.025)
mean(dat0$total_released)
quantile(dat0$total_released, probs=0.975)
quantile(dat0$total_released, probs=0.025)
mean(dat1$total_released)
quantile(dat1$total_released, probs=0.975)
quantile(dat1$total_released, probs=0.025)

## logistic generalised linear model (weighted)
# models
mod1 <- "persist ~ total_released"
mod2 <- "persist ~ 1"
## Make model vector
mod.vec <- c(mod1,mod2)
## Define n.mod
n.mod <- length(mod.vec)
# Model fitting and logLik output loop
Modnum <- length(mod.vec)
LL.vec <- SaveCount <- AICc.vec <- BIC.vec <- pc.dev.vec <- k.vec <- rep(0,Modnum)
mod.list <- list()
mod.num <- seq(1,Modnum,1)
for(i in 1:Modnum) {
  fit <- glm(as.formula(mod.vec[i]),family=binomial(link="logit"), data=dat, weights=longevMult, na.action=na.omit)
  assign(paste("fit",i,sep=""), fit)
  mod.list[[i]] <- fit
  print(i)
}
sumtable <- aicW(mod.list, finite = TRUE, null.model = NULL, order = F)
row.names(sumtable) <- mod.vec
summary.table <- sumtable[order(sumtable[,4],decreasing=F),1:9]
summary.table
## saturated residual diagnostic
i <- 1
fit <- glm(as.formula(mod.vec[i]),family=binomial(link="logit"), data=dat, weights=longevMult, na.action=na.omit)
plot(fit)


head(reviewdata)

min(reviewdata$total_released, na.rm = TRUE)
max(reviewdata$total_released, na.rm = TRUE)

surv.release.plot <- ggplot(reviewdata, aes(x = total_released, y = )) + 
  xlab("# of individuals released") + 
  ylab("# of translocations") +
  geom_histogram(colour = "black", fill = "grey", binwidth = 25) + 
  scale_x_continuous(limits = c(0,720), breaks = seq(0,700, by = 25),expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0,80, by = 10), expand = c(0,0))+
  Ctheme
surv.release.plot

