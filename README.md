# elliptic_k_using_agm_fortran
A Fortran library for computing the complete elliptic integral of the first kind $K(k)$ using AGM

```math
K(k) = \int_{ 0 }^{ \pi / 2 } \frac{ d \theta }{ \sqrt{ 1 - { k }^{ 2 } \sin^{ 2 } \theta } }
```

[![CI](https://github.com/dscf-1224/elliptic_k_using_agm_fortran/actions/workflows/ci.yml/badge.svg)](https://github.com/dscf-1224/elliptic_k_using_agm_fortran/actions/workflows/ci.yml)

## Features

- [x] Support for `real32`, `real64`, and `real128` precision
- [x] Elemental functions (works with scalars and arrays)

## Requirements

Fortran compiler with support for:
- `iso_fortran_env` intrinsic module
  - `real32`, `real64` types (required)
  - `real128` type (optional, compiler-dependent)
- `ieee_arithmetic` intrinsic module
  - `ieee_is_nan`
  - `ieee_positive_inf`
  - `ieee_quiet_nan`
  - `ieee_value`

Following `non_intrinsic` modules are also required
- [`arithmetic_geometric_mean_fortran`][arithmetic_geometric_mean_fortran]
- [`elliptic_nome_fortran`][elliptic_nome_fortran]

## Installation

### Option 1: Using fpm

To use this library within your [fpm](https://fpm.fortran-lang.org/) project, add the following to your package `fpm.toml` file:

```toml
[dependencies]
elliptic_k_using_agm_fortran = { git = "https://github.com/DSCF-1224/elliptic_k_using_agm_fortran.git" }
```

### Option 2: Manual Integration

1. Download `.f90` files in [`arithmetic_geometric_mean_fortran`][arithmetic_geometric_mean_fortran], [`elliptic_nome_fortran`][elliptic_nome_fortran] and [`elliptic_k_using_agm_fortran.f90`](src/elliptic_k_using_agm_fortran.f90)
2. Include them in your compilation


[arithmetic_geometric_mean_fortran]: https://github.com/DSCF-1224/arithmetic_geometric_mean_fortran
[elliptic_nome_fortran]: https://github.com/DSCF-1224/elliptic_nome_fortran

## Documentation 

[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://dscf-1224.github.io/elliptic_k_using_agm_fortran/)

<!-- EOF -->
