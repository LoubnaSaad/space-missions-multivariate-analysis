# Space Missions Multivariate Analysis

## Overview
Explored NASA space missions dataset to uncover patterns in mission profiles using clustering and factor analysis. Key questions: What drives diversity in missions (e.g., trade-offs in cost vs. efficiency)? What underlying factors link distance, fuel, payload, and success?

## Skills Demonstrated
- Data screening (outliers, normality via Shapiro/MVN).
- Visualization (GGally densities, boxplots, violin plots).
- Unsupervised: K-means clustering on scaled features; EFA for latent factors.
- Supervised: LDA and Random Forest for mission type classification (accuracy ~[add from script, e.g., 85%]).

## Key Findings
- 3 clusters: Short/low-cost Earth orbits vs. long/high-fuel deep space.
- Factors: "Efficiency" (negative load on fuel/cost) vs. "Scale" (positive on distance/payload).
- RF model: Payload weight most important predictor of mission type.

## Files
- `space_missions_analysis.R`: Full script (data load, descriptives, clustering, FA, LDA/RF).

## Report
- ((https://drive.google.com/file/d/1Q8yuivkqpsztSt2okNevmxe_2Job1L1x/view?usp=drive_link))

## Dataset
- Sourced from [https://www.kaggle.com/datasets/sameerk2004/space-missions-dataset)].
