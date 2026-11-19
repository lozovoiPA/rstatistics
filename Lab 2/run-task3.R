source("task3-1_5.R")
source("task3-6.R")
source("task3-7.R")
source("task3-8.R")

# Данные:
# Type, F - категориальные
# Rent, M2, Total - количественные
# Floor - порядковая

df <- import_df()

df$Rent <- as.numeric(df$Rent)
str(df)

df_clean <- clean_df(df)
cat("Было строк:", nrow(df), "Стало строк:", nrow(df_clean), "\n")

df <- df_clean
plot_hist(df)

test_shapiro(df$Rent, "Rent")
test_shapiro(df$M2, "M2")

# =============

plot_boxplots(df)

test_mann_whitney(Rent ~ Type, df, "Type", "Rent")
test_t_student(Rent ~ Type, df, "Type", "Rent")
test_mann_whitney(Rent ~ F, df, "F", "Rent")
test_mann_whitney(M2 ~ Type, df, "Type", "M2")

# =============

test_all_cor_spearman(df)

# =============

test_categorical()
