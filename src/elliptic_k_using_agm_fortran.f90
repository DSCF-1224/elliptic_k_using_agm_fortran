module elliptic_k_using_agm_fortran
    !! A Fortran library for computing the complete elliptic integral of the first kind using AGM

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_inf
    use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_value

    use, non_intrinsic :: arithmetic_geometric_mean_fortran



    implicit none



    private

    public :: elliptic_k



    real( real32  ), parameter :: half_pi_real32  = 1.57079632679489661923132169163975144210_real32
    real( real64  ), parameter :: half_pi_real64  = 1.57079632679489661923132169163975144210_real64
    real( real128 ), parameter :: half_pi_real128 = 1.57079632679489661923132169163975144210_real128



    interface elliptic_k
        module procedure :: elliptic_k_real32
        module procedure :: elliptic_k_real64
        module procedure :: elliptic_k_real128
    end interface elliptic_k



    contains



    elemental function elliptic_k_real32(k) result(integral)

        real(real32), intent(in) :: k !! elliptic modulus \( k \)



        real(real32) :: integral ! return value



        real(real32) :: abs_k
        real(real32) :: agm_k !! \( \text{AGM}( 1, { k }^{ \prime } ) \)
        real(real32) :: cmp_k !! \( { k }^{ \prime } := \sqrt{ 1 - { k }^{ 2 } } \)



        if ( ieee_is_nan(k) ) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

            return

        end if



        abs_k = abs(k)



        if (abs_k .gt. 1.0_real32) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

        else if (abs_k .lt. 1.0_real32) then

            cmp_k    = sqrt( (1.0_real32 - k) * (1.0_real32 + k) )
            agm_k    = arithmetic_geometric_mean_kernel( 1.0_real32, cmp_k )
            integral = half_pi_real32 / agm_k

        else

            integral = ieee_value( x = k, class = ieee_positive_inf )

        end if

    end function elliptic_k_real32



    elemental function elliptic_k_real64(k) result(integral)

        real(real64), intent(in) :: k !! elliptic modulus \( k \)



        real(real64) :: integral ! return value



        real(real64) :: abs_k
        real(real64) :: agm_k !! \( \text{AGM}( 1, { k }^{ \prime } ) \)
        real(real64) :: cmp_k !! \( { k }^{ \prime } := \sqrt{ 1 - { k }^{ 2 } } \)



        if ( ieee_is_nan(k) ) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

            return

        end if



        abs_k = abs(k)



        if (abs_k .gt. 1.0_real64) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

        else if (abs_k .lt. 1.0_real64) then

            cmp_k    = sqrt( (1.0_real64 - k) * (1.0_real64 + k) )
            agm_k    = arithmetic_geometric_mean_kernel( 1.0_real64, cmp_k )
            integral = half_pi_real64 / agm_k

        else

            integral = ieee_value( x = k, class = ieee_positive_inf )

        end if

    end function elliptic_k_real64



    elemental function elliptic_k_real128(k) result(integral)

        real(real128), intent(in) :: k !! elliptic modulus \( k \)



        real(real128) :: integral ! return value



        real(real128) :: abs_k
        real(real128) :: agm_k !! \( \text{AGM}( 1, { k }^{ \prime } ) \)
        real(real128) :: cmp_k !! \( { k }^{ \prime } := \sqrt{ 1 - { k }^{ 2 } } \)



        if ( ieee_is_nan(k) ) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

            return

        end if



        abs_k = abs(k)



        if (abs_k .gt. 1.0_real128) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

        else if (abs_k .lt. 1.0_real128) then

            cmp_k    = sqrt( (1.0_real128 - k) * (1.0_real128 + k) )
            agm_k    = arithmetic_geometric_mean_kernel( 1.0_real128, cmp_k )
            integral = half_pi_real128 / agm_k

        else

            integral = ieee_value( x = k, class = ieee_positive_inf )

        end if

    end function elliptic_k_real128

end module elliptic_k_using_agm_fortran
