####################
##
## Generation task
##
####################

load("~/Dropbox/Pudel/Pudel9/Daten/Pudel9.RData")
library(PudelHelper)

Daten.Gen[["id"]] <- as.integer(Daten.Gen[["Vp.Nr"]])
Daten.Gen[["excluded.id"]] <- as.integer(Daten.Gen[["Auswahl"]]=="Ausschluss")
Daten.Gen[["age"]] <- Daten.Gen[["Age"]]
Daten.Gen[["female"]] <- abs(as.integer(Daten.Gen[["Sex"]])-2)
Daten.Gen[["right.handed"]] <- abs(as.integer(Daten.Gen[["Hands"]])-2)
Daten.Gen[["color.blind"]] <- abs(as.integer(Daten.Gen[["Color"]])-1)
Daten.Gen[["1st.lang.german"]] <- abs(as.integer(Daten.Gen[["Sprache"]])-2)
Daten.Gen[["discipline"]] <- Daten.Gen[["Fach"]]


# independent variables
iv <- c("Material", "Condition", "Order", "PD instruction", "Block number")

Daten.Gen[["Material"]] <- factor(Daten.Gen[["Material"]], levels = c("A02", "A06"), labels = c("Random", "Regular"))
Daten.Gen[["Order"]] <- factor(Daten.Gen[["Reihenfolge"]], levels = c("InEx", "ExIn"), labels = c("Inclusion first", "Exclusion first"))
Daten.Gen[["PD instruction"]] <- factor(Daten.Gen[["Instruktion"]], levels = c("Inklusion", "Exklusion"), labels = c("Inclusion", "Exclusion"))
Daten.Gen[["Condition"]] <- factor(Daten.Gen[["InstrExpl"]], levels = c("Expl.No", "Expl.NoOne", "Expl.One", "Expl.OneOne", "Expl.PosOne"), labels = 1:5)
Daten.Gen[["Block number"]] <- as.integer(factor(Daten.Gen[["Block.Nr"]], levels = c("first", "second"), labels = c(1, 2)))
# trial info

trial <- c("Trial.Nr", "revealed")
Daten.Gen[["Trial"]] <- as.integer(Daten.Gen[["Trial.Nr"]])

## Daten.Gen[["revealed"]] <- factor(Daten.Gen[["instruiert.2C"]], levels = c("nicht.instruiert", "instruiert"), labels = c("non-revealed", "revealed"))


# response info

response <- c("RT", "vvR", "vR", "R", "vR.repetition", "repetition", "reversal", "FOC.correct")

Daten.Gen[["R"]] <- as.integer(Daten.Gen[["Reaktion"]])
Daten.Gen[["vR"]] <- pdl.vR(R = Daten.Gen[["R"]], Trial.Nr = Daten.Gen[["Trial.Nr"]])
Daten.Gen[["vvR"]] <- pdl.vvR(R = Daten.Gen[["R"]], Trial.Nr = Daten.Gen[["Trial.Nr"]])


Daten.Gen[["RT"]] <- Daten.Gen[["Reaktionszeit"]]
Daten.Gen[["vR.repetition"]] <- as.integer(Daten.Gen[["vvR"]] == Daten.Gen[["vR"]])
Daten.Gen[["repetition"]] <- as.integer(Daten.Gen[["R"]] == Daten.Gen[["vR"]])
Daten.Gen[["reversal"]] <- as.integer(Daten.Gen[["R"]] == Daten.Gen[["vvR"]])
Daten.Gen[["FOC.correct"]] <- Daten.Gen[["korrekt.seq"]]

# Generation <- Daten.Gen[,c("Exp.Name", demographics, iv, trial, response)]
# write.table(Generation,file="./ESM/generation.task.csv",sep=",",row.names=FALSE)

Generation <- Daten.Gen

Generation[["Explicit knowledge"]] <- factor(
  Generation[["InstrExpl"]]
  , levels = c("Expl.No", "Expl.One", "Expl.NoOne", "Expl.OneOne", "Expl.PosOne")
  , labels = c("nothing", "Practiced", "Unpracticed", "Practiced and unpracticed", "Unspecific practice")
)

Generation[["Transition type"]] <- factor(
  Generation[["vR.revealed"]]
  , levels = c("not", "postT.recently", "postT.before", "preT")
  , labels = c("non-revealed", "recently revealed, unpracticed", "revealed before, unpracticed", "practiced")
)


Generation[["Transition type"]] <- factor(
  Generation[["vR.revealed.12"]]
  , levels = c("not", "postT.fist", "postT.second", "preT")
  , labels = c("non-revealed", "first unpracticed", "second unpracticed", "practiced")
)


####################
##
## Acquisition task
##
####################


# demographics
demographics <- c("id","excluded.id", "age", "female", "right.handed", "color.blind", "1st.lang.german", "discipline")

Daten.Lrn[["id"]] <- as.integer(Daten.Lrn[["Vp.Nr"]])
Daten.Lrn[["excluded.id"]] <- as.integer(Daten.Lrn[["Auswahl"]]=="Ausschluss")
Daten.Lrn[["age"]] <- Daten.Lrn[["Age"]]
Daten.Lrn[["female"]] <- abs(as.integer(Daten.Lrn[["Sex"]])-2)
Daten.Lrn[["right.handed"]] <- abs(as.integer(Daten.Lrn[["Hands"]])-2)
Daten.Lrn[["color.blind"]] <- abs(as.integer(Daten.Lrn[["Color"]])-1)
Daten.Lrn[["1st.lang.german"]] <- abs(as.integer(Daten.Lrn[["Sprache"]])-2)
Daten.Lrn[["discipline"]] <- Daten.Lrn[["Fach"]]

# independent variables
iv <- c("Material", "Order")


# Daten.Lrn[["Material"]] <- factor(Daten.Lrn[["Material"]], levels = c("A02", "A06"), labels = c("Random", "Regular"))
Daten.Lrn[["Order"]] <- factor(Daten.Lrn[["Reihenfolge"]], levels=c("InEx", "ExIn"), labels=c("Inclusion first", "Exclusion first"))


# trial info

trial <- c("Block number", "Trial.Nr", "vvP", "vP", "P", "Transition status")

Daten.Lrn[["P"]] <- as.integer(Daten.Lrn[["Position"]])
Daten.Lrn[["vP"]] <- pdl.vR(R = Daten.Lrn[["P"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])
Daten.Lrn[["vvP"]] <- pdl.vvR(R = Daten.Lrn[["P"]], Trial.Nr = Daten.Lrn[["Trial.Nr"]])

Daten.Lrn[["Transition status"]] <- factor(Daten.Lrn[["Deviante.Position"]], levels = c("regelhaft", "Deviante"), labels = c("regular", "irregular"))
Daten.Lrn[["Block number"]] <- as.integer(Daten.Lrn[["Block.Nr"]])

# Daten.Lrn[["correct"]] <- as.integer(Daten.Lrn[["korrekt"]])

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
Acquisition <- Daten.Lrn

Acquisition[["Condition"]] <- factor(Acquisition$InstrExpl, levels = c("Expl.No", "Expl.NoOne", "Expl.One",  "Expl.OneOne",  "Expl.PosOne"), labels = as.character(c(1:5)))
Acquisition[["Trial"]] <- as.integer(Acquisition[["Trial.Nr"]])
Acquisition[["FOC transition status"]] <- Acquisition[["Transition status"]]

save(Generation, Acquisition, file = "data/pdl9.RData")
