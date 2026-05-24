# Probabilities-Statistics-Project
A R app designed to solve problems related to probabilities and statistics

## Discuție: Probabilități Teoretice vs. Probabilități Estimate prin Simulare

În cadrul rezolvării, evaluarea strategiilor de detecție s-a realizat prin metoda simulărilor (precum Monte Carlo) în detrimentul calculului analitic pur. Iată o comparație și discuție a diferențelor conceptuale:

**1. Probabilități Teoretice (Analitice)**
- Sunt obținute prin aplicarea directă a teoremelor matematice și a funcțiilor de masă ale distribuțiilor (ex. Hipergeometrică, Binomială).
- Reprezintă "valoarea adevărată" a unui eveniment asimptotic (dacă am repeta extragerile de o infinitate de ori).
- **Dezavantaj:** Devin extrem de greu (sau imposibil) de calculat manual în scenarii dinamice, cum este *Strategia B* (adaptivă), unde probabilitatea extragerilor este dependentă de o variabilă aleatoare compusă (media traficului fluctuează).

**2. Probabilități Estimate prin Simulare (Empirice)**
- Se bazează pe repetarea experimentului de *n* ori (aici minim 1000 de ani simulați) folosind un generator de numere pseudo-aleatoare.
- Ceea ce obținem este frecvența relativă a apariției evenimentului dorit.
- **Avantaj major:** Permite calcularea fără efort matematic a oricărui scenariu decizional complex (costuri penalizatoare, praguri if/else pe distribuții). De asemenea ne permite observarea **variabilității (deviația standard)**. Prin formulele teoretice găsim de obicei doar valoarea medie (Expected Value), dar într-un context de business, riscul (scenariul cel mai pesimist - "cozile distribuției") e la fel de important.

**Concluzie:** Conform *Legii Numerelor Mari*, probabilitățile empirice obținute converg spre probabilitățile teoretice o dată cu creșterea numărului de iterații (ex. $n=1000$). Metoda Monte Carlo, folosită pentru a estima costul mediu pe 1000 de iterații, ne oferă o dovadă probabilistică puternică a cărei strategii minimizează costul, integrând în același timp și evaluarea riscului (variabilitatea datelor).
