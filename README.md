# Machine learning predicts differences in the activity of AMPs between Gram-positive and Gram-negative bacteria

## Project Structure

```text
Gram-AMP-ML/
├── 1.模型1/
│   ├── group
│   └── matrix1
├── 2.模型2/
│   ├── group
│   └── matrix2
├── 3.模型3/
│   ├── group
│   └── matrix3
├── ML_predict.R


Requirements
R (≥ 4.0)

Required R packages:所需的 R 包：
randomForest
e1071
party
rpart

Notes：
Input data are provided as feature matrices (matrix*) and corresponding group labels (group).
File paths in ML_predict.R are relative; please keep the directory structure unchanged.
