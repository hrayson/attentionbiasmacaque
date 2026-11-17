# Early social adversity and attention bias in juvenile macaques

R code for processing attention-bias and social observation data and modelling their relationship to socioemotional behaviour.

## Paper

Rayson, H., Massera, A., Belluardo, M., Ben Hamed, S., & Ferrari, P. F. (2021). [Early social adversity modulates the relation between attention biases and socioemotional behaviour in juvenile macaques](https://doi.org/10.1038/s41598-021-00620-z). *Scientific Reports*.

## Code

- [`preprocessing/`](preprocessing/) contains data import and preparation scripts.
- [`attention_bias_analysis.R`](attention_bias_analysis.R) models attention bias. The `attention_bias_anxiety_analysis*.R` and `attention_bias_groom_analysis*.R` scripts examine associations with anxiety and grooming.
- The `*_3group.R` scripts use a three-group analysis.

Set the local data paths in the R scripts before running them. See [LICENSE](LICENSE) for the code licence.
