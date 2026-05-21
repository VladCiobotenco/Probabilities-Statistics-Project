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
  
  # --- DETECȚIE (Distribuție Hipergeometrică - extragere fără revenire) ---
  # rhyper(nr_zile, bile_albe(suspecte), bile_negre(normale), extrageri)
  detectate_A <- rhyper(zile_an, cereri_suspecte, cereri_normale, verificate_A)
  nedetectate_A <- cereri_suspecte - detectate_A
  
  detectate_B <- rhyper(zile_an, cereri_suspecte, cereri_normale, verificate_B)
  nedetectate_B <- cereri_suspecte - detectate_B
  
  # Asamblăm totul într-un dataframe
  data.frame(
    Ziua = 1:zile_an,
    Total_Cereri = total_cereri,
    Suspecte = cereri_suspecte,
    Normale = cereri_normale,
    P_Scenariu = as.factor(p_suspect),
    Verificate_A = verificate_A, Detectate_A = detectate_A, Nedetectate_A = nedetectate_A,
    Verificate_B = verificate_B, Detectate_B = detectate_B, Nedetectate_B = nedetectate_B
  )
}

# Rulăm simularea pentru cele 3 scenarii cerute
scenarii <- c(0.001, 0.005, 0.02)
# map_dfr aplică funcția pe fiecare probabilitate și lipește tabelele rezultate
date_simulare <- map_dfr(scenarii, simuleaza_scenariu)

# Transformăm formatul datelor ("long format") pentru a putea grupa strategiile A și B
date_long <- date_simulare %>%
  pivot_longer(
    cols = matches("_[AB]$"), 
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
    # Definiție: % Suspecte Detectate / % Din Trafic Verificat
    # Dacă verific 10% din trafic și prind 10% din hoți, eficiența e 1.
    # Dacă prind mai mulți hoți verificând mai puțin (datorită strategiei), eficiența crește.
    Eficienta = Prop_Medie_Detectate / mean(Verificate / Total_Cereri),
    .groups = "drop"
  )

print("--- Tabel Metrici de Performanta ---")
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
  scale_fill_manual(values = c("A" = "steelblue", "B" = "seagreen")) +
  labs(title = "Cererile Suspecte Detectate (A=Fix, B=Adaptiv)", x = "Nr. Detectări", y = "Frecvență")

# 6.3 Evoluția zilnică (Afișăm doar primele 60 de zile pentru lizibilitate)
plot_evolutie <- ggplot(date_plot %>% filter(Ziua <= 60), aes(x = Ziua)) +
  geom_line(aes(y = Suspecte, color = "Total Suspecte"), size = 1, linetype="dashed") +
  geom_line(aes(y = Detectate, color = Strategie), size = 1) +
  theme_minimal() +
  scale_color_manual(values = c("Total Suspecte" = "red", "A" = "steelblue", "B" = "seagreen")) +
  labs(title = "Evoluția Zilnică (Primele 60 zile)", x = "Ziua din an", y = "Număr Cereri")

# 6.4 Grafic Comparativ - Indicatorul de Eficiență pe toate cele 3 scenarii
plot_eficienta <- ggplot(metrici, aes(x = P_Scenariu, y = Eficienta, fill = Strategie)) +
  geom_col(position = "dodge", color="black") +
  theme_minimal() +
  scale_fill_manual(values = c("A" = "steelblue", "B" = "seagreen")) +
  labs(title = "Eficiența Strategiilor (Randament)", x = "Probabilitate (p)", y = "Indicator Eficiență")

# Afișarea graficelor (necesită pachetul 'patchwork')
# Combinație elegantă într-o singură imagine
(plot_suspecte | plot_detectate) / (plot_evolutie | plot_eficienta)