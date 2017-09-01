
dimnames(theta) <- list(
  "iteration" = NULL
  , "id" = NULL
  , "parX" = c("ci_nv", "ce_nv", "ai_nv", "ae_nv", "ci_v", "ce_v", "ai_v", "ae_v")
)

theta <- apply(X = theta, MARGIN = c("id", "parX"), FUN = mean)

theta <- melt(theta)

theta$parameter[grepl(theta$parX, pattern = "c")] <- "c"
theta$parameter[grepl(theta$parX, pattern = "a")] <- "a"

theta$`PD instruction`[grepl(theta$parX, pattern = "i")] <- "Inclusion"
theta$`PD instruction`[grepl(theta$parX, pattern = "e")] <- "Exclusion"

theta$`PD instruction` <- factor(theta$`PD instruction`, levels = c("Inclusion", "Exclusion"))

theta$`Transition type` <- "revealed"
theta$`Transition type`[grepl(theta$parX, pattern = "_nv")] <- "non-revealed"

theta$Material <- "Probabilistic"
theta$Material[(theta$id < 31)|(theta$id > 60 & theta$id < 91)] <- "Random"

theta$Material <- factor(theta$Material, levels = c("Random", "Probabilistic"))

theta$Condition <- "One transition revealed"
theta$Condition[theta$id < 61] <- "No transition revealed"

theta$`Transition type`[theta$Condition=="No transition revealed"]<- "non-revealed"

