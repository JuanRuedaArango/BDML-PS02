# ============================================================
# 10. Predicción sobre test
# ============================================================
predictSample <- test %>%
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")) %>%
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) %>%
  select(id, pobre)

head(predictSample)

# ============================================================
# 11. Guardar archivo de envío
# ============================================================
name <- make_submission_name(model1)
write.csv(predictSample, name, row.names = FALSE)

cat("Archivo guardado:", name, "\n")
