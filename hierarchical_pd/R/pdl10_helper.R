
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

material <- data.frame(
  "id" = 1:171
  , "Material" = c(rep(1, 29), rep(2, 30), rep(3, 30), rep(1, 28), rep(2, 26), rep(3, 28))
)

theta$Material <- NA
theta$Material <- material$Material
table(theta$Material)/8

theta$Material <- factor(theta$Material, levels = 1:3, labels = c("Random", "Mixed SOC", "Pure SOC"))

theta$Condition <- "No transition revealed"
theta$Condition[theta$id > 89] <- "Two transitions revealed"

theta$`Transition type`[theta$Condition=="No transition revealed"]<- "non-revealed"

