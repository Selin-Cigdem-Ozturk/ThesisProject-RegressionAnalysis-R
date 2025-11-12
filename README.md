Regression Analysis

To explore the relationships between employee perceptions of AI integration and sociodemographic or organizational characteristics, multiple linear regression analyses were conducted in R. 
The models examined five key dependent variables—Decision-Making, Autonomy, Trust, Fairness, and Worry—derived from the OECD (2022) Worker Survey indices. 
Independent variables included sector, gender, age, education, company size, and job role.


Each model followed the general form 𝑌𝑖 = β0 + β1Sectori + β2Genderi + β3Agei + β4Educationi + β5CompanySizei + β6JobRolei + ϵi
	​
Missing data were handled through complete case analysis after attempts at multiple imputation using the mice package proved unfeasible due to structural inconsistencies across countries.

Diagnostic tests confirmed that multicollinearity, residual normality, and linearity assumptions were largely met. 
The regression results provided insights into how organizational context and individual characteristics shape perceptions of AI-driven decision-making, autonomy, fairness, trust, and worry, revealing significant cross-sectoral differences between manufacturing and finance employees.
