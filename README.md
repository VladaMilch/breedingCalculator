# breedingCalculator
R package to plan animal breedings: sample size calculation with the probability of success and offspring genotypes parameters


## Installation 

```{r, eval = FALSE}
library(devtools)
devtools::install_github("VladaMilch/breedingCalculator")
```


## Usage

### 1. Offsprings of a single genotype


When the breeding set up is aimed at offsprings of a single genotype, 
or simply a total number of born pups, one may use the following function to 
calculate the required number of breedings. 

Here we calculate how many FVB/N mouse matings are required to ensure that 
at least 20 pups are born within a period of 3 days, 
with the success probability of 90%. 


```{r single genotype simple}
n_breedings_single <- singleGenotype(
  confidence_p = 0.90,
  birth_days = 3,
  n_offsprings = 20, 
  sex_distribution = "unimportant",
  desired_genotype_p = 0.25,
  strain = "FVB/N")

print(n_breedings_single)
```

Note that in the example above, both male and female pups are suitable for the 
breeding experiment. 


In case an experiment requires the same number of male and female pups, 
one can use the following input:
`sex_distribution = "balanced"`. 

In case all pups must be male or all female, 
use `sex_distribution = "all one sex"`.

```{r single balanced}
n_breedings_single <- singleGenotype(
  confidence_p = 0.90,
  birth_days = 3,
  n_offsprings = 20, 
  sex_distribution = "balanced",
  desired_genotype_p = 0.25,
  strain = "FVB/N")

print(n_breedings_single)
```


### 2. 
