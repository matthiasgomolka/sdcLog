data("sdc_descriptives_DT")
sdc_descriptives(data = sdc_descriptives_DT, id_var = "id", val_var = "val_1")

data("sdc_extreme_DT")
sdc_extreme(data = sdc_extreme_DT, id_var = "id", val_var = "val_1")

data("sdc_model_DT")
model_1 <- lm(y ~ x_1 + x_2, data = sdc_model_DT)
sdc_model(data = sdc_model_DT, model = model_1, id_var = "id")
