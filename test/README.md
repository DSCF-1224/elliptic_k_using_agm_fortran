# How to generate `reference?.csv`

## [`reference0.csv`](reference0.csv)

```Mathematica
          Power[1/Sqrt[2], Range[1, 127]]
EllipticK[Power[1/2      , Range[1, 127]]]
```

## [`reference1.csv`](reference1.csv)

```Mathematica
                Sum[Power[1/2, i], {i, 1, Range[114]}]
EllipticK[Power[Sum[Power[1/2, i], {i, 1, Range[114]}], 2]]
```

## How to format

```Mathematica
ScientificForm[N[%,39], NumberFormat -> (Row[{#1, "e", #3}] &)]
```
