####################
##
## Generation task
##
####################

load("~/Dropbox/Pudel/Pudel7/Daten/Pudel7.RData")
library(PudelHelper)

# demographics
demographics <- c("id","excluded.id", "age", "female", "right.handed", "color.blind", "1st.lang.german", "discipline")

Daten.Gen[["excluded.id"]] <- 0
Daten.Gen[["age"]] <- Daten.Gen[["Age"]]
Daten.Gen[["female"]] <- abs(as.integer(Daten.Gen[["Sex"]])-2)
Daten.Gen[["right.handed"]] <- abs(as.integer(Daten.Gen[["Hands"]])-2)
Daten.Gen[["color.blind"]] <- abs(as.integer(Daten.Gen[["Color"]])-1)
Daten.Gen[["1st.lang.german"]] <- abs(as.integer(Daten.Gen[["Sprache"]])-2)
Daten.Gen[["discipline"]] <- Daten.Gen[["Fach"]]

# independent variables
iv <- c("Material", "Condition", "Order", "PD instruction", "Block number")

# Daten.Gen[["Material"]] <- factor(Daten.Gen[["Material"]], levels = c("A02", "A06"), labels = c("Random", "Probabilistic"))
Daten.Gen[["Order"]] <- factor(Daten.Gen[["Reihenfolge"]], levels = c("InEx", "ExIn"), labels = c("Inclusion first", "Exclusion first"))
Daten.Gen[["PD instruction"]] <- factor(Daten.Gen[["Instruktion"]], levels = c("Inklusion", "Exklusion"), labels = c("Inclusion", "Exclusion"))
Daten.Gen[["Condition"]] <- factor(Daten.Gen[["InstrExpl"]], levels = c("Expl.No", "Expl.One"), labels = c("No transition revealed", "One transition revealed"))
Daten.Gen[["Block number"]] <- as.integer(factor(Daten.Gen[["Block.Nr"]], levels = c("first", "second"), labels = c(1, 2)))

# trial info

trial <- c("Trial", "revealed")

Daten.Gen[["revealed"]] <- factor(Daten.Gen[["instruiert.2C"]], levels = c("nicht.instruiert", "instruiert"), labels = c("non-revealed", "revealed"))
Daten.Gen[["Trial"]] <- as.integer(Daten.Gen[["Trial.Nr"]])


# response info

response <- c("RT", "vvR", "vR", "R", "vR.repetition", "repetition", "reversal", "FOC.correct")

Daten.Gen[["R"]] <- as.integer(Daten.Gen[["Reaktion"]])
Daten.Gen[["vR"]] <- pdl.vR(R = Daten.Gen[["R"]], Trial.Nr = Daten.Gen[["Trial.Nr"]])
Daten.Gen[["vvR"]] <- pdl.vvR(R = Daten.Gen[["R"]], Trial.Nr = Daten.Gen[["Trial.Nr"]])


Daten.Gen[["RT"]] <- Daten.Gen[["Reaktionszeit"]]
Daten.Gen[["vR.repetition"]] <- as.integer(Daten.Gen[["vvR"]] == Daten.Gen[["vR"]])
Daten.Gen[["repetition"]] <- as.integer(Daten.Gen[["R"]] == Daten.Gen[["vR"]])
Daten.Gen[["reversal"]] <- as.integer(Daten.Gen[["R"]] == Daten.Gen[["vvR"]])
Daten.Gen[["FOC.correct"]] <- Daten.Gen[["korrekt.fitted"]]

Generation <- Daten.Gen# [,c("Exp.Name", demographics, iv, trial, response)]
# write.table(Generation,file="./ESM/generation.task.csv",sep=",",row.names=FALSE)


####################
##
## Acquisition task
##
####################


# demographics
demographics <- c("id","excluded.id", "age", "female", "right.handed", "color.blind", "1st.lang.german", "discipline")

Daten.Lrn[["id"]] <- as.integer(Daten.Lrn[["Vp.Nr"]])
Daten.Lrn[["excluded.id"]] <- 0
Daten.Lrn[["age"]] <- Daten.Lrn[["Age"]]
Daten.Lrn[["female"]] <- abs(as.integer(Daten.Lrn[["Sex"]])-2)
Daten.Lrn[["right.handed"]] <- abs(as.integer(Daten.Lrn[["Hands"]])-2)
Daten.Lrn[["color.blind"]] <- abs(as.integer(Daten.Lrn[["Color"]])-1)
Daten.Lrn[["1st.lang.german"]] <- abs(as.integer(Daten.Lrn[["Sprache"]])-2)
Daten.Lrn[["discipline"]] <- Daten.Lrn[["Fach"]]

# independent variables
iv <- c("Material", "Order")


Daten.Lrn[["Material"]] <- factor(Daten.Lrn[["Material"]], levels = c("A02", "A06"), labels = c("Random", "Probabilistic"))
Daten.Lrn[["Order"]] <- factor(Daten.Lrn[["Reihenfolge"]], levels=c("InEx", "ExIn"), labels=c("Inclusion first", "Exclusion first"))


# trial info

trial <- c("Block number", "Trial", "vvP", "vP", "P", "FOC transition status")

Daten.Lrn[["P"]] <- as.integer(Daten.Lrn[["Position"]])
Daten.Lrn[["vP"]] <- pdl.vR(R = Daten.Lrn[["P"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])
Daten.Lrn[["vvP"]] <- pdl.vvR(R = Daten.Lrn[["P"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])
Daten.Lrn[["Block number"]] <- as.integer(Daten.Lrn[["Block.Nr"]])
Daten.Lrn[["Trial"]] <- as.integer(Daten.Lrn[["Trial.Nr"]])
Daten.Lrn[["FOC transition status"]] <- factor(Daten.Lrn[["FOC transition status"]], levels = 1:0, labels = c("regular", "irregular"))

# response info

response <- c("RT","SRI","vvR", "vR", "R", "vR.repetition", "repetition", "reversal", "vR.error","error")

Daten.Lrn[["RT"]] <- Daten.Lrn[["Reaktionszeit"]]
Daten.Lrn[["SRI"]] <- Daten.Lrn[["Reaktionszeit"]]-250

Daten.Lrn[["R"]] <- as.integer(Daten.Lrn[["Reaktion"]])
Daten.Lrn[["vR"]] <- pdl.vR(R = Daten.Lrn[["R"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])
Daten.Lrn[["vvR"]] <- pdl.vvR(R = Daten.Lrn[["R"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])

Daten.Lrn[["vR.repetition"]] <- as.integer(Daten.Lrn[["vvR"]] == Daten.Lrn[["vR"]])
Daten.Lrn[["repetition"]] <- as.integer(Daten.Lrn[["R"]] == Daten.Lrn[["vR"]])
Daten.Lrn[["reversal"]] <- as.integer(Daten.Lrn[["R"]] == Daten.Lrn[["vvR"]])
Daten.Lrn[["error"]] <- as.integer(Daten.Lrn[["R"]]!=Daten.Lrn[["P"]])
Daten.Lrn[["vR.error"]] <- pdl.vR(Daten.Lrn[["error"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])

Acquisition <- Daten.Lrn[,c("Exp.Name", demographics, iv, trial, response)]
# write.table(Acquisition,file="./ESM/acquisition.task.csv",sep=",",row.names=FALSE)



Daten.PW<-read.table(file="~/Dropbox/Pudel/Pudel7/Daten/Daten.PW.csv",sep=",",header=TRUE)
Daten.SR<-read.table(file="~/Dropbox/Pudel/Pudel7/Daten/Daten.SR.csv",sep=",",header=TRUE)
Korrelationen.1st<-read.table(file="~/Dropbox/Pudel/Pudel7/Daten/Korrelationen.1st.csv",sep=",",header=TRUE)
cor.1st.nicht.instruiert<-read.table(file="~/Dropbox/Pudel/Pudel7/Daten/cor.1st.nicht.instruiert.csv",sep=",",header=TRUE)

Daten.PW[["id"]] <- as.integer(Daten.PW[["Vp.Nr"]])
Daten.SR[["id"]] <- as.integer(Daten.SR[["Vp.Nr"]])
Korrelationen.1st[["id"]] <- as.integer(Korrelationen.1st[["Vp.Nr"]])
cor.1st.nicht.instruiert[["id"]] <- as.integer(cor.1st.nicht.instruiert[["Vp.Nr"]])

## Daten.SR

# demographics
# demographics <- c("id","excluded.id", "age", "female", "right.handed", "color.blind", "1st.lang.german", "discipline")
# 
# Daten.SR[["excluded.id"]] <- 0
# Daten.SR[["age"]] <- Daten.SR[["Age"]]
# Daten.SR[["female"]] <- abs(as.integer(Daten.SR[["Sex"]])-2)
# Daten.SR[["right.handed"]] <- abs(as.integer(Daten.SR[["Hands"]])-2)
# Daten.SR[["color.blind"]] <- abs(as.integer(Daten.SR[["Color"]])-1)
# Daten.SR[["1st.lang.german"]] <- abs(as.integer(Daten.SR[["Sprache"]])-2)
# Daten.SR[["discipline"]] <- Daten.SR[["Fach"]]

# independent variables

iv <- c("Material", "Condition", "Order", "PD instruction")

Daten.SR[["Material"]] <- factor(Daten.SR[["Material"]], levels = c("A02", "A06"), labels = c("Random", "Probabilistic"))
Daten.SR[["Order"]] <- factor(Daten.SR[["Reihenfolge"]], levels = c("InEx", "ExIn"), labels = c("Inclusion first", "Exclusion first"))
Daten.SR[["Condition"]] <- factor(Daten.SR[["InstrExpl"]], levels = c("Expl.No", "Expl.One"), labels = c("No transition revealed", "One transition revealed"))
Daten.SR[["vR"]] <- as.integer(Daten.SR$vorige.Reaktion)




Daten.SR$R <- as.integer(Daten.SR$Reaktion)
Daten.SR$id <- as.integer(Daten.SR$Vp.Nr)

Generation$FC <- NA

for (i in 1:nrow(Daten.SR)) {
  n <- Daten.SR$id[i]
  j <- Daten.SR$vR[i]
  k <- Daten.SR$R[i]
  x <- ifelse(Daten.SR$forced.choice[i]=="FC.genannt", 1, 0)
  Generation[!is.na(Generation$vR)&Generation$id==n&Generation$vR==j&Generation$R==k, "FC"] <- x
}

Generation$FC2 <- NA

for (i in 1:nrow(SelfReport)) {
  n <- SelfReport[["id"]][i]
  j <- SelfReport$vR[i]
  k <- SelfReport$R[i]
  x <- SelfReport$adhers.FC[i]
  
  Generation[!is.na(Generation$vR)&Generation$id==n&Generation$vR==j&Generation$R==k, "FC2"] <- x
}

Generation$free2 <- NA

for (i in 1:nrow(SelfReport)) {
  n <- SelfReport[["id"]][i]
  j <- SelfReport$vR[i]
  k <- SelfReport$R[i]
  x <- SelfReport$adhers.free[i]
  
  Generation[!is.na(Generation$vR)&Generation$id==n&Generation$vR==j&Generation$R==k, "free2"] <- x
}
# table(Generation$free2, Generation$SR.frei.3C, useNA="ifany")

# Generation[is.na(Generation$free2)&Generation$SR.frei.3C=="nicht.genannt", ]

Korrelationen.1st[["Material"]] <- factor(Korrelationen.1st[["Material"]], levels = c("A02", "A06"), labels = c("Random", "Probabilistic"))
Korrelationen.1st[["Order"]] <- factor(Korrelationen.1st[["Reihenfolge"]], levels = c("InEx", "ExIn"), labels = c("Inclusion first", "Exclusion first"))
Korrelationen.1st[["Condition"]] <- factor(Korrelationen.1st[["InstrExpl"]], levels = c("Expl.No", "Expl.One"), labels = c("No transition revealed", "One transition revealed"))

cor.1st.nicht.instruiert[["Material"]] <- factor(cor.1st.nicht.instruiert[["Material"]], levels = c("A02", "A06"), labels = c("Random", "Probabilistic"))
cor.1st.nicht.instruiert[["Order"]] <- factor(cor.1st.nicht.instruiert[["Reihenfolge"]], levels = c("InEx", "ExIn"), labels = c("Inclusion first", "Exclusion first"))
cor.1st.nicht.instruiert[["Condition"]] <- factor(cor.1st.nicht.instruiert[["InstrExpl"]], levels = c("Expl.No", "Expl.One"), labels = c("No transition revealed", "One transition revealed"))

save(Daten.Gen, Daten.PW, Daten.SR, SelfReport, Korrelationen.1st, cor.1st.nicht.instruiert, Generation, Acquisition, SelfReport, SelfReport_aggregated, file = "data/pdl7.RData")

