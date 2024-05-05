# Calcular el coeficiente de variación para el gemelo 2 ("HRWAGEL")
CV <- sd(db_twins$HRWAGEL)/mean(db_twins$HRWAGEL)
CV

# Calcular Q1 para el gemelo 1 ("HRWAGEH")
Q1_1 <- quantile(db_twins$HRWAGEH, 0.25)
Q1_1

# Calcular Q1 para el gemelo 2 ("HRWAGEL")
Q1_2 <- quantile(db_twins$HRWAGEL, 0.25)
Q1_2

# Calcular Q2 para el gemelo 1 ("HRWAGEH")
Q2_1 <- quantile(db_twins$HRWAGEH, 0.50)
Q2_1

# Calcular Q2 para el gemelo 2 ("HRWAGEL")
Q2_2 <- quantile(db_twins$HRWAGEL, 0.50)
Q2_2

# Calcular Q3 para el gemelo 1 ("HRWAGEH")
Q3_1 <- quantile(db_twins$HRWAGEH, 0.75)
Q3_1

# Calcular Q3 para el gemelo 2 ("HRWAGEL")
Q3_2 <- quantile(db_twins$HRWAGEL, 0.75)
Q3_2

# Calcular el mínimo para el gemelo 1 ("HRWAGEH")
min(db_twins$HRWAGEH)

# Calcular el mínimo para el gemelo 2 ("HRWAGEL")
min(db_twins$HRWAGEL)

# Calcular el máximo para el gemelo 1 ("HRWAGEH")
max(db_twins$HRWAGEH)

# Calcular el máximo para el gemelo 2 ("HRWAGEL")
max(db_twins$HRWAGEL)
