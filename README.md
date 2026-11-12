# NHL Playoffs Classification Models

An R project comparing classification models that predict whether an NHL team makes the playoffs from its wins, losses, goals scored, goals allowed and points. It uses the Lahman hockey `Teams` dataset.

## Repository Contents

| File | Description |
|---|---|
| `NHL playoff accuracy.R` | R script: data prep and six classification models |
| `Teams.csv` | Team-season hockey data (1,518 rows) |

## Commit History

### `d69413e`: Upload of the R script and dataset (2022-05-18)
- **Data preparation (dplyr):**
  - filtered to NHL seasons only
  - selected the key variables
  - created a true/false (1/0) playoff-appearance target
  - scaled the features
- **All NHL teams, model comparison:**

  | Model | Accuracy |
  |---|---|
  | Logistic regression | 86.9% |
  | SVM | 85.4% |
  | Kernel SVM | 86.7% |
  | Naive Bayes | 78.1% |
  | Decision tree | 85.9% |
  | **Random forest** | **89.7%** |

- **Washington Capitals only (a much smaller sample):**

  | Model | Accuracy |
  |---|---|
  | KNN | 100% |
  | Kernel SVM, decision tree, random forest | 90.9% |
  | SVM | 81.8% |
  | Naive Bayes | 72.7% |

## Tools
R, RStudio, dplyr, caTools, class, e1071, rpart, randomForest
