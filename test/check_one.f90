program check_one

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan

    use, non_intrinsic :: elliptic_k_using_agm_fortran

    use, non_intrinsic :: ieee_class_fortran



    implicit none



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine test_real32

        if ( .not. is_ieee_positive_inf( elliptic_k(1.0_real32) ) ) error stop

    end subroutine test_real32



    subroutine test_real64

        if ( .not. is_ieee_positive_inf( elliptic_k(1.0_real64) ) ) error stop

    end subroutine test_real64



    subroutine test_real128

        if ( .not. is_ieee_positive_inf( elliptic_k(1.0_real128) ) ) error stop

    end subroutine test_real128

end program check_one
