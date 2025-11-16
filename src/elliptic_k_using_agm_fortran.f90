module elliptic_k_using_agm_fortran
    !! A Fortran library for computing the complete elliptic integral of the first kind using AGM

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: arithmetic_geometric_mean_fortran



    implicit none



    private



    real( real32  ), parameter :: half_pi_real32  = 1.57079632679489661923132169163975144210_real32
    real( real64  ), parameter :: half_pi_real64  = 1.57079632679489661923132169163975144210_real64
    real( real128 ), parameter :: half_pi_real128 = 1.57079632679489661923132169163975144210_real128

end module elliptic_k_using_agm_fortran
