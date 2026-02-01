# Gram-Amp-ML
Machine learning predicts differences in the activity of AMP between Gram-positive and Gram-negative bacteria.

Gram-AMP-ML/
├── 1.模型1/               # Model 1: 3-mer Frequency Features
│   ├── group            
│   └── matrix1     
│
├── 2.模型/                # Model 2: Physicochemical Property Features (Best Model)
│   ├── group            
│   └── matrix2     
│
├── 3.模型3/               # Model 3: Hybrid Features (3-mer + Physicochemical)
│   ├── group        
│   └── matrix3
├── ML_predict.R

The project uses a unified script (`ML_predict.R`) located in the directory to analyze data distributed across three model folders.
