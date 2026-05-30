# Instalare pachete (dacă nu le aveți deja)
# install.packages(c("tidyverse", "patchwork"))

library(tidyverse)
library(patchwork)

# Setăm seed-ul pentru ca rezultatele să fie identice la fiecare rulare
set.seed(2026)

# ==============================================================================
# 1 & 2. PARAMETRII GENERALI ȘI GENERAREA TRAFICULUI
# ==============================================================================
zile_an <- 365
media_zilnica <- 5000 
dispersia_nb <- 5 # Parametru pentru a crea fluctuații mari de trafic (zile foarte aglomerate)

# --- ANALIZA COSTURILOR (Parametri) ---
# Alocăm un cost per verificare și o penalizare mai mare pentru fiecare suspect nedetectat
cost_verificare <- 2      
cost_nedetectat <- 200    

# Generăm traficul total pentru un an (Folosim Binomială Negativă)
total_cereri <- rnbinom(zile_an, size = dispersia_nb, mu = media_zilnica)

# ==============================================================================
# 3 & 4. FUNCȚIA DE SIMULARE (Pentru scenariile p și Strategii)
# ==============================================================================
simuleaza_scenariu <- function(p_suspect) {
  
  # Generăm cererile suspecte din traficul total (Distribuție Binomială)
  cereri_suspecte <- rbinom(zile_an, size = total_cereri, prob = p_suspect)
  cereri_normale  <- total_cereri - cereri_suspecte
  
  # --- STRATEGIA A: Aleatoare Simplă (Fix 10% din trafic) ---
  pct_fix <- 0.10
  verificate_A <- round(total_cereri * pct_fix)
  
  # --- STRATEGIA B: Adaptivă ---
  # Verificăm 5% în zilele liniștite și 20% în zilele cu trafic mai mare decât media
  pct_adaptiv <- ifelse(total_cereri > mean(total_cereri), 0.20, 0.05)
  verificate_B <- round(total_cereri * pct_adaptiv)


  # STRATEGIA C: Numar maxim de cereri verificate
  nr_maxim_cereri <- 1000
  verificate_C <- pmin(nr_maxim_cereri, total_cereri)
  
  # --- DETECȚIE (Distribuție Hipergeometrică - extragere fără revenire) ---
  # rhyper(nr_zile, bile_albe(suspecte), bile_negre(normale), extrageri)
  detectate_A <- rhyper(zile_an, cereri_suspecte, cereri_normale, verificate_A)
  nedetectate_A <- cereri_suspecte - detectate_A
  
  detectate_B <- rhyper(zile_an, cereri_suspecte, cereri_normale, verificate_B)
  nedetectate_B <- cereri_suspecte - detectate_B

  detectate_C <- rhyper(zile_an, cereri_suspecte, cereri_normale, verificate_C)
  nedetectate_C <- cereri_suspecte - detectate_C
  
  # Asamblăm totul într-un dataframe
  data.frame(
    Ziua = 1:zile_an,
    Total_Cereri = total_cereri,
    Suspecte = cereri_suspecte,
    Normale = cereri_normale,
    P_Scenariu = as.factor(p_suspect),
    Verificate_A = verificate_A, Detectate_A = detectate_A, Nedetectate_A = nedetectate_A,
    Verificate_B = verificate_B, Detectate_B = detectate_B, Nedetectate_B = nedetectate_B,
    Verificate_C = verificate_C, Detectate_C = detectate_C, Nedetectate_C = nedetectate_C
  )
}

# Rulăm simularea pentru cele 3 scenarii cerute
scenarii <- c(0.001, 0.005, 0.02)
# map_dfr aplică funcția pe fiecare probabilitate și lipește tabelele rezultate
date_simulare <- map_dfr(scenarii, simuleaza_scenariu)

# Transformăm formatul datelor ("long format") pentru a putea grupa strategiile A și B
date_long <- date_simulare %>%
  pivot_longer(
    cols = matches("_[ABC]$"), 
    names_to = c(".value", "Strategie"), 
    names_pattern = "(.*)_(.)"
  )

# ==============================================================================
# 5. CALCULUL METRICILOR (pe Scenariu și Strategie)
# ==============================================================================
metrici <- date_long %>%
  group_by(P_Scenariu, Strategie) %>%
  summarise(
    # Prob. empirică de a detecta cel puțin 1 pe zi (Zile cu detecție / 365)
    Prob_Detectie_1_Zi = mean(Detectate > 0),
    
    # Proporții (evităm împărțirea la 0 pentru zilele perfect curate)
    Prop_Medie_Detectate = mean(ifelse(Suspecte == 0, 0, Detectate / Suspecte)),
    Prop_Medie_Nedetectate = mean(ifelse(Suspecte == 0, 0, Nedetectate / Suspecte)),
    Medie_Verificari = mean(Verificate),
    
    # INDICATOR DE EFICIENȚĂ PROPUS: "Randamentul Detecției"
    Eficienta = Prop_Medie_Detectate / mean(Verificate / Total_Cereri),
    
    # ANALIZA COSTURILOR
    #Cost_Verificari = sum(Verificate) * cost_verificare,
    #Cost_Penalizari = sum(Nedetectate) * cost_nedetectat,
    #Cost_Total = Cost_Verificari + Cost_Penalizari,
    .groups = "drop"
  )

print("--- Tabel Metrici de Performanta (Inclusiv Costuri) ---")
print(metrici)

# ==============================================================================
# 6. REPREZENTĂRI GRAFICE (Folosim p=0.005 pentru claritate)
# ==============================================================================
date_plot <- date_long %>% filter(P_Scenariu == "0.005")

# 6.1 Histograma cererilor suspecte zilnice
plot_suspecte <- ggplot(date_plot, aes(x = Suspecte)) +
  geom_histogram(fill = "tomato", color = "black", binwidth = 2) +
  theme_minimal() +
  labs(title = "Distribuția cererilor suspecte/zi (p=0.005)", x = "Nr. Cereri Suspecte", y = "Frecvență (Zile)")

# 6.2 Histograma detectărilor (Comparativ A vs B)
plot_detectate <- ggplot(date_plot, aes(x = Detectate, fill = Strategie)) +
  geom_histogram(position = "identity", alpha = 0.6, binwidth = 1, color="black") +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "steelblue", "B" = "seagreen", "C" = "salmon")) +
  labs(title = "Cererile Suspecte Detectate (A=Fix(Procent), B=Adaptiv, C=Fix(Numar)", x = "Nr. Detectări", y = "Frecvență")

# 6.3 Evoluția zilnică (Afișăm doar primele 60 de zile pentru lizibilitate)
plot_evolutie <- ggplot(date_plot %>% filter(Ziua <= 60), aes(x = Ziua)) +
  geom_line(aes(y = Suspecte, color = "Total Suspecte"), linewidth = 1, linetype="dashed") +
  geom_line(aes(y = Detectate, color = Strategie), linewidth = 1) +
  theme_minimal() +
  scale_color_manual(values = c("Total Suspecte" = "red", "A" = "steelblue", "B" = "seagreen", "C" = "salmon")) +
  labs(title = "Evoluția Zilnică (Primele 60 zile)", x = "Ziua din an", y = "Număr Cereri")

# 6.4 Grafic Comparativ - Indicatorul de Eficiență pe toate cele 3 scenarii
plot_eficienta <- ggplot(metrici, aes(x = P_Scenariu, y = Eficienta, fill = Strategie)) +
  geom_col(position = "dodge", color="black") +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "steelblue", "B" = "seagreen", "C" = "salmon")) +
  labs(title = "Eficiența Strategiilor (Randament)", x = "Probabilitate (p)", y = "Indicator Eficiență")

# 6.5 Grafic Comparativ - Cost Total
plot_cost <- ggplot(metrici, aes(x = P_Scenariu, y = Cost_Total, fill = Strategie)) +
  geom_col(position = "dodge", color="black") +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "steelblue", "B" = "seagreen", "C" = "salmon")) +
  labs(title = "Costul Total (Verificări + Penalizări)", x = "Probabilitate (p)", y = "Cost Total")

# (plot_suspecte | plot_detectate) / (plot_evolutie | plot_eficienta) / plot_cost

# ==============================================================================
# 7. SIMULAREA PENTRU MULTIPLE PROCENTE DE VERIFICARE
# ==============================================================================

p_suspect_test <- 0.005 
cereri_suspecte_globale <- rbinom(zile_an, size = total_cereri, prob = p_suspect_test)
cereri_normale_globale  <- total_cereri - cereri_suspecte_globale

# Definim vectorul de procente cerut
procente_verificare <- c(0.01, 0.05, 0.10, 0.20, 0.30)

# Aplicăm o funcție iterativă (map_dfr) pentru a calcula metricile fiecărui procent
rezultate_procente <- map_dfr(procente_verificare, function(pct) {
  
  # Calculăm câte cereri sunt verificate efectiv pe zi
  verificate <- round(total_cereri * pct)
  
  # Simulăm detecția folosind variabilele globale corectate
  detectate <- rhyper(zile_an, cereri_suspecte_globale, cereri_normale_globale, verificate)
  
  # Returnăm un rând de tabel cu rezultatele
  data.frame(
    Procent_Verificare = paste0(pct * 100, "%"),
    Prob_Detectie_Min_1 = mean(detectate > 0),
    Proportie_Medie_Detectata = mean(ifelse(cereri_suspecte_globale == 0, 0, detectate / cereri_suspecte_globale))
  )
})

# Afișăm tabelul în consolă
print("--- Efectul creșterii procentului de verificare ---")
print(rezultate_procente)

# Transformăm coloana Procent în număr pentru grafic
grafic_data <- rezultate_procente %>%
  mutate(Procent_Numeric = as.numeric(gsub("%", "", Procent_Verificare)))

ggplot(grafic_data, aes(x = Procent_Numeric)) +
  geom_line(aes(y = Prob_Detectie_Min_1, color = "Probabilitate Detecție Zilnică"), linewidth = 1.2) +
  geom_point(aes(y = Prob_Detectie_Min_1), size = 3, color = "darkred") +
  geom_line(aes(y = Proportie_Medie_Detectata, color = "Proporție Suspecți Prinși"), linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = Proportie_Medie_Detectata), size = 3, color = "steelblue") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Efectul creșterii efortului de verificare",
    subtitle = "Probabilitate de detecție vs. Proporția efectivă detectată",
    x = "Procent de verificare din traficul total (%)",
    y = "Probabilitatea de detectie",
    color = "Metrică"
  )


# 8. SIMULARE MONTE CARLO (Minim 1000 iteratii pentru a studia variabilitatea)
N_sim <- 1000
p_mc <- 0.005

rezultate_mc <- map_dfr(1:N_sim, function(i) {
  
  # Regenerăm traficul pentru fiecare iterație
  trafic_mc <- rnbinom(zile_an, size = dispersia_nb, mu = media_zilnica)
  suspecte_mc <- rbinom(zile_an, size = trafic_mc, prob = p_mc)
  normale_mc  <- trafic_mc - suspecte_mc
  
  # A: Strategie 10% Fix
  verificate_A <- round(trafic_mc * 0.10)
  detectate_A <- rhyper(zile_an, suspecte_mc, normale_mc, verificate_A)
  cost_A <- sum(verificate_A) * cost_verificare + sum(suspecte_mc - detectate_A) * cost_nedetectat
  
  # B: Strategie Adaptivă
  pct_adaptiv <- ifelse(trafic_mc > mean(trafic_mc), 0.20, 0.05)
  verificate_B <- round(trafic_mc * pct_adaptiv)
  detectate_B <- rhyper(zile_an, suspecte_mc, normale_mc, verificate_B)
  cost_B <- sum(verificate_B) * cost_verificare + sum(suspecte_mc - detectate_B) * cost_nedetectat
  
  # C: Numar maxim de cereri verificate
  verificate_C <- pmin(1000, trafic_mc)
  detectate_C <- rhyper(zile_an, suspecte_mc, normale_mc, verificate_C)   
  cost_C <- sum(verificate_C) * cost_verificare + sum(suspecte_mc - detectate_C) * cost_nedetectat

  
  data.frame(Simulare = i, Cost_A = cost_A, Cost_B = cost_B, Cost_C = cost_C)
})

# Analiza mediilor și a variabilității (Deviația Standard)
rezumat_mc <- rezultate_mc %>%
  pivot_longer(cols = c(Cost_A, Cost_B, Cost_C), names_to = "Strategie", values_to = "Cost_Total") %>%
  group_by(Strategie) %>%
  summarise(
    Medie_Cost = mean(Cost_Total),
    Variabilitate_SD = sd(Cost_Total), # Deviația standard ne arată fluctuațiile
    Cost_Min = min(Cost_Total),
    Cost_Max = max(Cost_Total)
  )

print("--- Analiza Monte Carlo: Media și Variabilitatea Costurilor ---")
print(rezumat_mc)

# Generare Boxplot
ggplot(rezultate_mc %>% pivot_longer(cols = c(Cost_A, Cost_B,Cost_C), names_to = "Strategie", values_to = "Cost_Total"), 
       aes(x = Strategie, y = Cost_Total, fill = Strategie)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  theme_minimal() +
  scale_fill_manual(values = c("Cost_A" = "steelblue", "Cost_B" = "seagreen", "Cost_C" = "salmon")) +
  labs(title = paste("Variabilitatea Costului Total Anual (Monte Carlo -", N_sim, "simulări)"),
       subtitle = "Punctele din afara cutiilor reprezintă ani cu deviații extreme (outliers)",
       x = "Strategie Aplicată", y = "Cost Total")