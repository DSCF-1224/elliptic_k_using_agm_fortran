# How to generate `reference?.csv`

## [`reference0.csv`](reference0.csv)

```Mathematica
Power[1/Sqrt[2], Range[1, 127]]
 EllipticK[Power[1/2, Range[1, 127]]]
```

```Mathematica
ScientificForm[N[%,39], NumberFormat -> (Row[{#1, "e", #3}] &)]
```
