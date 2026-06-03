# Human-Scream-Pattern-Recognition

# 🔊 Do Human Screams Permit Individual Recognition? - Replication Study

This repository contains a full replication of Engelberg, Schwartz, and Gouzoules (2019), examining whether humans can identify individuals based on nonlinguistic vocalizations—specifically screams. Our study investigates how scream-based identification varies based on **vocalizer gender** and **listener gender**, and uses signal detection theory to calculate sensitivity (d′ scores).

## 🧠 Overview

Human screams are emotionally salient vocalizations that may carry identity-relevant acoustic cues. This study explores:
- Can people distinguish between different screamers?
- Does vocalizer gender influence recognizability?
- Do listener gender differences affect perception or accuracy?

We replicate prior findings while extending analysis using mixed-factor ANOVA and trial-level latency measures.

---
```
## 📁 Repository Structure

/ 📂 data/

└── Human_Scream_Data.csv # Cleaned dataset used for replication analysis

📂 scripts/
└── scream_analysis.R # Full R script used for cleaning, analysis, and statistics

📂 report/
└── Do human screams permit individual recognition? - Replication.pdf # Final report
```

---

## 🧪 Methodology

**Participants:**  
104 college students (73 female, 41 male) from Emory University.

**Stimuli:**  
58 trials consisting of scream pairs from movies, TV, and internet sources—each pair classified as:
- Same Vocalizer
- Different Vocalizer
- Duration Modified

**Procedure:**  
Participants judged whether each scream pair came from the same individual or not. Response accuracy and latency were recorded. Premature responses (< -10ms after second scream onset) were excluded.

---

## 🔢 Signal Detection Analysis (d′)

- **Hit Rate**: Correctly identifying two screams from the same person
- **False Alarm Rate**: Incorrectly identifying two different vocalizers as the same
- **d′ Score**: Computed as `qnorm(HitRate) - qnorm(FalseAlarmRate)`, adjusted to avoid infinite z-scores

We conducted analyses:
- Across all trials
- Separately for **female vocalizers** and **male vocalizers**
- Examining listener gender interaction effects

---

## 📊 Key Findings

- **Average d′ Score**: 1.63 (indicating strong discriminability)
- **Vocalizer Gender**:
  - Male vocalizers were more easily recognized (mean d′ = 1.72)
  - Female vocalizers were less distinguishable (mean d′ = 1.55)
- **Listener Gender**:
  - No significant effect on d′ scores or response latencies
- **Latency**:
  - Slower response times were observed for male vocalizers

---

## 💻 Script: `scream_analysis.R`

This script replicates the full statistical analysis reported in the study.

### 🔍 Main Features:
- Filters duplicate trials and premature responses
- Maps trial types based on `StimType`
- Calculates hit and false alarm rates for each subject
- Computes d′ scores using signal detection theory
- Adjusts for infinite z-scores using bounded estimates
- Runs two-way mixed-factor ANOVA for:
  - d′ scores
  - Mean response latency
- Separates analysis by vocalizer gender (male vs. female)

### 📚 References

- Engelberg, Schwartz, & Gouzoules (2019)

- Schwartz, J. W., & Gouzoules, H. (2019). Decoding human screams: perception of emotional arousal from pitch and duration.

- Anderson, R. C., & Klofstad, C. A. (2012). Preference for Leaders with Masculine Voices Holds.

- Altrov et al. (2013). The Role of Empathy in the Recognition of Vocal Emotions.

- Lavan et al. (2018). Impoverished encoding of speaker identity in spontaneous laughter.

- Schaerlaeken & Grandjean (2018). Affect bursts decoding in humans.

👥 Authors

Gunjan Anand, Cheyenne Bridge, Enoghayin Imasuen, Saffanat Sumra, Fausto Vaca
Mentors: Agnes Zhao, Jessica Jones
Undergraduate Laboratory at Berkeley, Psychology and Cognitive Science Division

### 🧰 R Packages Used:
```r
library(dplyr)
library(tidyr)
library(psych)
library(nlme)
library(car) 
