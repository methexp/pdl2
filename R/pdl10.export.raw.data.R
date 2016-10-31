####################
##
## Generation task
##
####################

load("~/Dropbox/Pudel/Pudel10/Daten/Pudel10.RData")
library(PudelHelper)

demographics <- c("id", "excluded.id", "age", "female", "right.handed", "color.blind", "1st.lang.german", "discipline")

Daten.Gen[["id"]] <- as.integer(Daten.Gen[["Vp.Nr"]])
Daten.Gen[["excluded.id"]] <- as.integer(Daten.Gen[["faulty.instructions"]])
Daten.Gen[["age"]] <- Daten.Gen[["Age"]]
Daten.Gen[["female"]] <- abs(as.integer(Daten.Gen[["Sex"]])-2)
Daten.Gen[["right.handed"]] <- abs(as.integer(Daten.Gen[["Hands"]])-2)
Daten.Gen[["color.blind"]] <- abs(as.integer(Daten.Gen[["Color"]])-1)
Daten.Gen[["1st.lang.german"]] <- abs(as.integer(Daten.Gen[["Sprache"]])-2)
Daten.Gen[["discipline"]] <- Daten.Gen[["Fach"]]


# independent variables
iv <- c("Material", "Condition", "Order", "PD instruction", "Block number")

# Daten.Gen[["Material"]] <- factor(Daten.Gen[["Material"]], levels = c("A02", "A06"), labels = c("Random", "Regular"))
Daten.Gen[["Order"]] <- factor(Daten.Gen[["Reihenfolge"]], levels = c("InEx", "ExIn"), labels = c("Inclusion first", "Exclusion first"))
Daten.Gen[["PD instruction"]] <- factor(Daten.Gen[["Instruktion"]], levels = c("Inklusion", "Exklusion"), labels = c("Inclusion", "Exclusion"))
Daten.Gen[["Condition"]] <- factor(Daten.Gen[["InstrExpl"]], levels = c("Expl.No", "Expl.One"), labels = c("No transition revealed", "Two transitions revealed"))
Daten.Gen[["Block number"]] <- as.integer(factor(Daten.Gen[["Block.Nr"]], levels = c("first", "second"), labels = c(1, 2)))

# trial info

trial <- c("Trial", "revealed")

Daten.Gen[["Trial"]] <- as.integer(Daten.Gen[["Trial.Nr"]])
Daten.Gen[["revealed"]] <- Daten.Gen[["instruiert"]]


# response info

response <- c("RT", "vvR", "vR", "R", "vR.repetition", "repetition", "reversal", "FOC.correct", "SOC.correct")

Daten.Gen[["R"]] <- as.integer(Daten.Gen[["Reaktion"]])
Daten.Gen[["vR"]] <- pdl.vR(R = Daten.Gen[["R"]], Trial.Nr = Daten.Gen[["Trial.Nr"]])
Daten.Gen[["vvR"]] <- pdl.vvR(R = Daten.Gen[["R"]], Trial.Nr = Daten.Gen[["Trial.Nr"]])

Daten.Gen[["RT"]] <- Daten.Gen[["Reaktionszeit"]]
Daten.Gen[["vR.repetition"]] <- as.integer(Daten.Gen[["vvR"]] == Daten.Gen[["vR"]])
Daten.Gen[["repetition"]] <- as.integer(Daten.Gen[["R"]] == Daten.Gen[["vR"]])
Daten.Gen[["reversal"]] <- as.integer(Daten.Gen[["R"]] == Daten.Gen[["vvR"]])
Daten.Gen[["FOC.correct"]] <- Daten.Gen[["korrekt.foc.2L"]]
Daten.Gen[["SOC.correct"]] <- Daten.Gen[["korrekt.2nd"]]

Generation <- Daten.Gen

## write to .csv and .RData-files
write.table(Generation[,c(demographics, iv, trial, response)], file="./data/pdl10.generation.task.csv",sep=",",row.names=FALSE)

####################
##
## Acquisition task
##
####################


# demographics
demographics <- c("id", "age", "female", "right.handed", "color.blind", "1st.lang.german", "discipline")

Daten.Lrn[["id"]] <- as.integer(Daten.Lrn[["Vp.Nr"]])
Daten.Lrn[["excluded.id"]] <- 0
Daten.Lrn[["age"]] <- Daten.Lrn[["Age"]]
Daten.Lrn[["female"]] <- abs(as.integer(Daten.Lrn[["Sex"]])-2)
Daten.Lrn[["right.handed"]] <- abs(as.integer(Daten.Lrn[["Hands"]])-2)
Daten.Lrn[["color.blind"]] <- abs(as.integer(Daten.Lrn[["Color"]])-1)
Daten.Lrn[["1st.lang.german"]] <- abs(as.integer(Daten.Lrn[["Sprache"]])-2)
Daten.Lrn[["discipline"]] <- Daten.Lrn[["Fach"]]

# independent variables
iv <- c("Material", "Condition", "Order")


# Daten.Lrn[["Material"]] <- factor(Daten.Lrn[["Material"]], levels = c("A02", "A06"), labels = c("Random", "Regular"))
Daten.Lrn[["Order"]] <- factor(Daten.Lrn[["Reihenfolge"]], levels=c("InEx", "ExIn"), labels=c("Inclusion first", "Exclusion first"))
Daten.Lrn[["Condition"]] <- factor(Daten.Lrn[["InstrExpl"]], levels = c("Expl.No", "Expl.One"), labels = c("No transition revealed", "Two transitions revealed"))


# trial info

trial <- c("Block number", "Trial", "vvP", "vP", "P", "SOC transition status", "FOC transition status")

Daten.Lrn[["Trial"]] <- as.integer(Daten.Lrn[["Trial.Nr"]])

Daten.Lrn[["P"]] <- as.integer(Daten.Lrn[["Position"]])
Daten.Lrn[["vP"]] <- pdl.vR(R = Daten.Lrn[["P"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])
Daten.Lrn[["vvP"]] <- pdl.vvR(R = Daten.Lrn[["P"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])

Daten.Lrn[["SOC transition status"]] <- factor(Daten.Lrn[["korrekt.2nd.P"]], levels = c(1, 0), labels = c("regular", "irregular"))
Daten.Lrn[["FOC transition status"]] <- factor(Daten.Lrn[["korrekt.foc.g"]], levels = 0:2, labels = c("low", "mid", "high"))


Daten.Lrn[["Block number"]] <- as.integer(Daten.Lrn[["Block.Nr"]])


# response info

response <- c("RT","SRI","vvR", "vR", "R", "vR.repetition", "repetition", "reversal", "vR.error","error")

Daten.Lrn[["RT"]] <- Daten.Lrn[["Reaktionszeit"]]
Daten.Lrn[["SRI"]] <- Daten.Lrn[["Reaktionszeit"]]

Daten.Lrn[["R"]] <- as.integer(Daten.Lrn[["Reaktion"]])
Daten.Lrn[["vR"]] <- pdl.vR(R = Daten.Lrn[["R"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])
Daten.Lrn[["vvR"]] <- pdl.vvR(R = Daten.Lrn[["R"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])

Daten.Lrn[["vR.repetition"]] <- as.integer(Daten.Lrn[["vvR"]] == Daten.Lrn[["vR"]])
Daten.Lrn[["repetition"]] <- as.integer(Daten.Lrn[["R"]] == Daten.Lrn[["vR"]])
Daten.Lrn[["reversal"]] <- as.integer(Daten.Lrn[["R"]] == Daten.Lrn[["vvR"]])
Daten.Lrn[["error"]] <- as.integer(Daten.Lrn[["R"]]!=Daten.Lrn[["P"]])
Daten.Lrn[["vR.error"]] <- as.integer(Daten.Lrn[["vR"]] != Daten.Lrn[["vP"]]) # pdl.vR(Daten.Lrn[["error"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])
Daten.Lrn[["vvR.error"]] <- as.integer(Daten.Lrn[["vvR"]] != Daten.Lrn[["vvP"]])

# Acquisition <- Daten.Lrn[,c("Exp.Name", demographics, iv, trial, response)]
Acquisition <- Daten.Lrn


## write to .csv and .RData-files
write.table(Acquisition[,c(demographics, iv, trial, response)], file="./data/pdl10.acquisition.task.csv",sep=",",row.names=FALSE)

save(Generation, Acquisition, file = "data/pdl10.RData")
