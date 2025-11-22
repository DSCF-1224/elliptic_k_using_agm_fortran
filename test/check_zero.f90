program check_one

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: elliptic_k_using_agm_fortran

    use, non_intrinsic :: ieee_class_fortran



    implicit none



    real( real32  ), parameter :: half_pi_real32  = 1.57079632679489661923132169163975144210_real32
    real( real64  ), parameter :: half_pi_real64  = 1.57079632679489661923132169163975144210_real64
    real( real128 ), parameter :: half_pi_real128 = 1.57079632679489661923132169163975144210_real128



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine test_real32

        real(real32) :: err



        err = elliptic_k(0.0_real32) - half_pi_real32

        if ( .not. is_ieee_positive_zero(err) ) error stop

    end subroutine test_real32



    subroutine test_real64

        real(real64) :: err



        err = elliptic_k(0.0_real64) - half_pi_real64

        if ( .not. is_ieee_positive_zero(err) ) error stop

    end subroutine test_real64



    subroutine test_real128

        real(real128) :: err



        err = elliptic_k(0.0_real128) - half_pi_real128

        if ( .not. is_ieee_positive_zero(err) ) error stop

    end subroutine test_real128

end program check_one
