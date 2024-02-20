library(foreign)
library(scales)
library(ggplot2)
library(gRbase)
library(gRain)
library(gRim)
library(bnlearn)
library(igraph)

# Carico il dataset
jobs <- read.table("C:\\Users\\Cavicchioli\\Desktop\\Cavicchioli_Michael_7149344\\JOBSII.txt", header=T)

# Sostituisco i valori NA con -1, per non avere problemi in seguito
jobs$D[is.na(jobs$D)] <- -1

# Estrapolo le colonne di interesse
job <- jobs[c(1,2,3,4,7:10, 18:20)]


# ANALISI PRELIMINARE
# Distribuzione dell'età
hist(job$age, main="Distribuzione dell'età", xlab="Età", ylab="#.",
     xlim = c(0, 100), col="lightblue")

# Imposta la griglia dei grafici su una riga e due colonne
par(mfrow=c(1, 2))  

# Grafico a barre per la variabile sesso
barplot(table(job$sex), main="Distribuzione per Sesso", xlab="Sesso", ylab="#.", 
        names.arg = c("M", "F"),
        col="lightblue")

# Grafico a barre per la variabile etnia
barplot(table(job$race), main="Distribuzione per Etnia", xlab="Etnia", ylab="#.", 
        names.arg = c("Bianca", "Altro"),
        col="lightgreen")

# Ripristina la griglia dei grafici
par(mfrow=c(1, 1))

# Distribuzione della depressione per età
plot(job$age, job$depress6, xlab = "Eta'", ylab = "Liv. Depr.",
     main = "Distribuzione della depressione per età",
     col = "Red", pch = 16, cex = 2)


# Estraggo i valori per le categorie specificate
valori <- c(sum(job$Z==1 & job$D==1 & job$race==1),  
            sum(job$Z==1 & job$D==1 & job$race==0),  
            sum(job$Z==1 & job$D==0 & job$race==1),  
            sum(job$Z==1 & job$D==0 & job$race==0),  
            sum(job$Z==1 & job$race==1),  
            sum(job$Z==1 & job$race==0)) 

# Etichette per le fette
etichette <- c("Z=1, D=1, race=1",
            "Z=1, D=1, race=0",
            "Z=1, D=0, race=1",
            "Z=1, D=0, race=0",
            "Z=0, race=1",
            "Z=0, race=0")

percentuali <- percent(valori / sum(valori))

pie(valori, labels = percentuali,col = rainbow(length(valori)))
legend(x=1.2, y=0.8, horiz = FALSE, text.width = 0.4, legend = etichette, cex=0.6,
       fill = rainbow(length(valori)))

# Titolo
title("Distribuzione del trattamento per le etnie")

# Interazione tra trattamento, depressione e rischio
dep6 <- as.numeric(job$depress6 <= 1.5)
ggplot(job, aes(x = Z, y = dep6, color = factor(Risk))) +
  geom_point(position = position_jitter(width = 0.2), size = 3) +
  labs(title = "Interazione tra Trattamento, Depressione e Rischio",
       x = "Trattamento (0 = non assegnato, 1 = assegnato)", 
       y = "Depressione (0 = Alta, 1 = Bassa)",
       color = "Rischio (0=Basso, 1=Alto)") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Interazione tra trattamento, alcol e reimpiego
ggplot(job, aes(x = factor(Z), y = alcohol6, fill = factor(employ6))) +
  geom_boxplot() +
  labs(title = "Interazione tra Trattamento, Alcol e Reimpiego",
       x = "Trattamento (0 = non assegnato, 1 = assegnato)",
       y = "Livelli di Alcol",
       fill = "Reimpiego dopo 6 mesi") +
  scale_fill_manual(values = c("red", "blue"), breaks = c(0, 1), labels = c("No", "Si")) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


# REGRESSIONE LOGISTICA
# Reimpiego dopo 6 mesi
job$alcohol6 = as.numeric(job$alcohol6<=4.5)
job$depress6 = as.numeric(job$depress6<=1.5)

fit <- glm(job$employ6 ~ job$Risk + job$sex + job$age + job$race + job$Z + job$D 
           + job$nonmarried + job$alcohol6 + job$depress6, family = binomial)
summary(fit)

fit_intercetta <- glm(job$employ6 ~ 1, family = binomial)
summary(fit_intercetta)

# BACKWARD - VM
back_mv <- step(fit, scope = formula(fit_intercetta), direction = "backward", k = 0)
summary(back_mv)
print(BIC(back_mv))

# BACKWARD - AIC
back_aic <- step(fit, scope = formula(fit_intercetta), direction = "backward", k = 2)
summary(back_aic)
print(BIC(back_aic))

# BACKWARD - BIC
back_bic <- step(fit, scope = formula(fit_intercetta), direction = "backward", k = log(length(job$employ6)))
summary(back_bic)
print(BIC(back_bic))

# FORWARD - VM
forw_vm <- step(fit_intercetta, scope = formula(fit), direction = "forward", k = 0)
summary(forw_vm)
print(BIC(forw_vm))

# FORWARD - AIC
forw_aic <- step(fit_intercetta, scope = formula(fit), direction = "forward", k = 2)
summary(forw_aic)
print(BIC(forw_aic))

# FORWARD - BIC
forw_bic <- step(fit_intercetta, scope = formula(fit), direction = "forward", k = log(length(job$employ6)))
summary(forw_bic)
print(BIC(forw_bic))

# BOTH - VM
both_vm <- step(fit, scope = formula(fit), direction = "both", k = 0)
summary(both_vm)
print(BIC(both_vm))

# BOTH - AIC
both_aic <- step(fit, scope = formula(fit), direction = "both", k = 2)
summary(both_aic)
print(BIC(both_aic))

# BOTH - BIC
both_bic <- step(fit, scope = formula(fit), direction = "both", k = log(length(job$employ6)))
summary(both_bic)
print(BIC(both_bic))

# Coefficienti BACK_BIC
print(coefficients(back_bic))


# Depressione dopo 6 mesi
fit <- glm(job$depress6 ~ job$Risk + job$sex + job$age + job$race + job$Z + job$D 
           + job$nonmarried + job$alcohol6 + job$employ6, family = binomial)
summary(fit)

fit_intercetta <- glm(job$depress6 ~ 1, family = binomial)
summary(fit_intercetta)

# BACKWARD - VM
back_mv <- step(fit, scope = formula(fit_intercetta), direction = "backward", k = 0)
summary(back_mv)
print(BIC(back_mv))

# BACKWARD - AIC
back_aic <- step(fit, scope = formula(fit_intercetta), direction = "backward", k = 2)
summary(back_aic)
print(BIC(back_aic))

# BACKWARD - BIC
back_bic <- step(fit, scope = formula(fit_intercetta), direction = "backward", k = log(length(job$depress6)))
summary(back_bic)
print(BIC(back_bic))

# FORWARD - VM
forw_vm <- step(fit_intercetta, scope = formula(fit), direction = "forward", k = 0)
summary(forw_vm)
print(BIC(forw_vm))

# FORWARD - AIC
forw_aic <- step(fit_intercetta, scope = formula(fit), direction = "forward", k = 2)
summary(forw_aic)
print(BIC(forw_aic))

# FORWARD - BIC
forw_bic <- step(fit_intercetta, scope = formula(fit), direction = "forward", k = log(length(job$depress6)))
summary(forw_bic)
print(BIC(forw_bic))

# BOTH - VM
both_vm <- step(fit, scope = formula(fit), direction = "both", k = 0)
summary(both_vm)
print(BIC(both_vm))

# BOTH - AIC
both_aic <- step(fit, scope = formula(fit), direction = "both", k = 2)
summary(both_aic)
print(BIC(both_aic))

# BOTH - BIC
both_bic <- step(fit, scope = formula(fit), direction = "both", k = log(length(job$depress6)))
summary(both_bic)
print(BIC(both_bic))

# Coefficienti BACK_BIC
print(coefficients(back_bic))


# RETE BAYESIANA E DAG
# Rimozione colonna ID
tutte_colonne <- colnames(job)
colonna_da_escludere <- c("ID")
colonne_da_mantenere <- setdiff(tutte_colonne, colonna_da_escludere)
job_senza_id <- job[, colonne_da_mantenere, drop = FALSE]
colnames(job)
colnames(job_senza_id)

# Normalizzazione dati
job_senza_id$Risk <- as.factor(job_senza_id$Risk)
job_senza_id$Z <- as.factor(job_senza_id$Z)
job_senza_id$sex <- as.factor(job_senza_id$sex)
job_senza_id$race <- as.factor(job_senza_id$race)
job_senza_id$nonmarried <- as.factor(job_senza_id$nonmarried)
job_senza_id$employ6 <- as.factor(job_senza_id$employ6)
job_senza_id$depress6 <- as.factor(job_senza_id$depress6)

# Creazione rete bayesiana
job_bn <- hc(job_senza_id)
plot(job_bn)
print(arcs(job_bn))

# Matrice con tutti 0
blM <- matrix(0, nrow=10, ncol=10)
rownames(blM) <- colnames(blM) <- names(job_senza_id)
block <- c(2, 1, 1, 2, 2, 2, 2, 0, 0, 0)

# M[i,j] = 1 -> arco da i a j
for (b in 0:2) blM[block < b, block==b] <- 1

# Creazione Blacklist -> archi che non si desidera
blackL <- data.frame(as_edgelist(as(blM, "igraph")))
names(blackL) <- c("from", "to")

# Creazione rete bayesiana dai dati, specificando quali archi non si desidera
job_bn <- hc(job_senza_id, blacklist = blackL)
plot(job_bn)

# Aggiungere alla BL l'arco: Nonmarried -> Age 
blackL <- rbind(blackL, c("nonmarried", "age"))

# Nuova rete bayesiana da BL aggiornata
job_bn <- hc(job_senza_id, blacklist = blackL)
plot(job_bn)

# Regressioni basate sulla rete bayesiana
job_bn_employ6 <- glm(employ6 ~ depress6, family = binomial, data = job_senza_id)
summary(job_bn_employ6)
print(BIC(job_bn_employ6))

job_bn_depress6 <- glm(depress6 ~ Risk, family = binomial, data = job_senza_id)
summary(job_bn_depress6)
print(BIC(job_bn_depress6))

job_bn_alcohol6 <- glm(alcohol6 ~ sex + race, family = binomial, data = job_senza_id)
summary(job_bn_alcohol6)
print(BIC(job_bn_alcohol6))

job_bn_employ6 <- glm(employ6 ~ depress6 + age + race + Z, family = binomial, data = job_senza_id)
summary(job_bn_employ6)
print(BIC(job_bn_employ6))

job_bn_depress6 <- glm(depress6 ~ Risk + employ6 + D, family = binomial, data = job_senza_id)
summary(job_bn_depress6)
print(BIC(job_bn_depress6))

# Creazione DAG
job_dag <- dag(~ D * age,
               ~ Risk * nonmarried,
               ~ depress6 * Risk,
               ~ employ6 * depress6,
               ~ alcohol6 * race,
               ~ alcohol6 * sex,
               ~ nonmarried * sex
               )
plot(job_dag)

# Rete bayesiana dal DAG
job_bn_da_dag <- grain(job_dag, data = job_senza_id)
job_bn_da_dag <- compile(job_bn_da_dag, propagate = TRUE, smooth=0.1)

# Probabilità marginali
querygrain(job_bn_da_dag, nodes = c("sex"), type = "marginal") 
querygrain(job_bn_da_dag, nodes = c("Risk"), type = "marginal") 

# Probabilità congiunte
querygrain(job_bn_da_dag, nodes = c("sex", "nonmarried"), type = "joint") 
querygrain(job_bn_da_dag, nodes = c("race", "alcohol6"), type = "joint") 
querygrain(job_bn_da_dag, nodes = c("Risk", "depress6"), type = "joint")

# Probabilità condizionali
querygrain(job_bn_da_dag, nodes = c("employ6", "depress6"), type = "conditional")
querygrain(job_bn_da_dag, nodes = c("depress6", "Risk"), type = "conditional")