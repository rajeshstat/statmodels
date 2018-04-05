# statmodels
Comprehensive statistical models

Install this package using the command 
```
devtools::install_github("rajeshstat/statmodels")
```

## Comprehensive alternative to t.test
```
## Test example for t_test_dumb
t_test_dumb(iris %>% filter(Species !="setosa"), Petal.Width, Species, var.equal = T)

```
