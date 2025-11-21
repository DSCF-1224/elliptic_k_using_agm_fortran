program check_nan

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

        real(real32) :: k



        call set_ieee_quiet_nan(k)

        if ( .not. ieee_is_nan( elliptic_k(k) ) ) error stop

    end subroutine test_real32



    subroutine test_real64

        real(real64) :: k



        call set_ieee_quiet_nan(k)

        if ( .not. ieee_is_nan( elliptic_k(k) ) ) error stop

    end subroutine test_real64



    subroutine test_real128

        real(real128) :: k



        call set_ieee_quiet_nan(k)

        if ( .not. ieee_is_nan( elliptic_k(k) ) ) error stop

    end subroutine test_real128

end program check_nan
