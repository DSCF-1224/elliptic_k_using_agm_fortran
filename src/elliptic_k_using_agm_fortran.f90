module elliptic_k_using_agm_fortran
    !! A Fortran library for computing the complete elliptic integral of the first kind \( K(k) \)
    !! using arithmetic-geometric mean (AGM)
    !! 
    !! \( K(k) \) is defined as
    !! $$
    !! K (k) := \int_{ 0 }^{ \pi / 2 } \frac{ d \theta }{ \sqrt{ 1 - { k }^{ 2 } \sin^{ 2 } \theta } } 
    !! $$
    !! where \( k \) is the elliptic modulus.

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_inf
    use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_value

    use, non_intrinsic :: arithmetic_geometric_mean_fortran

    use, non_intrinsic :: elliptic_nome_fortran



    implicit none



    private

    public :: elliptic_k



    real( real32  ), parameter :: half_pi_real32  = 1.57079632679489661923132169163975144210_real32
    real( real64  ), parameter :: half_pi_real64  = 1.57079632679489661923132169163975144210_real64
    real( real128 ), parameter :: half_pi_real128 = 1.57079632679489661923132169163975144210_real128

    real( real32  ), parameter :: mns_inv_pi_real32  = -0.318309886183790671537767526745028724069_real32
    real( real64  ), parameter :: mns_inv_pi_real64  = -0.318309886183790671537767526745028724069_real64
    real( real128 ), parameter :: mns_inv_pi_real128 = -0.318309886183790671537767526745028724069_real128




    interface elliptic_k
        !! Compute the complete elliptic integral of the first kind \( K(k) \) using AGM
        !! 
        !! @warning
        !! - The elliptic modulus \( k \) must satisfy \( |k| \le 1 \)
        !! - Returns `ieee_quiet_nan`    for \( |k| > 1 \)
        !! - Returns `ieee_positive_inf` for \( |k| = 1 \)
        !! @endwarning
        module procedure :: elliptic_k_real32
        module procedure :: elliptic_k_real64
        module procedure :: elliptic_k_real128
    end interface elliptic_k



    contains



    elemental function elliptic_k_real32(k) result(integral)
        !! Compute the complete elliptic integral of the first kind \( K(k) \) using AGM
        !! 
        !! @warning
        !! - The elliptic modulus \( k \) must satisfy \( |k| \le 1 \)
        !! - Returns `ieee_quiet_nan`    for \( |k| > 1 \)
        !! - Returns `ieee_positive_inf` for \( |k| = 1 \)
        !! @endwarning
        !! 
        !! @note
        !! Dependencies:
        !! [`arithmetic_geometric_mean_kernel`](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/interface/arithmetic_geometric_mean_kernel.html),
        !! [`elliptic_nome_auto`](https://dscf-1224.github.io/elliptic_nome_fortran/interface/elliptic_nome_auto.html)
        !! @endnote

        real(real32), intent(in) :: k !! elliptic modulus \( k \)



        real(real32) :: integral !! \( K(k) \)



        real(real32) :: abs_k
        real(real32) :: agm_k !! \( \text{AGM}( 1, { k }^{ \prime } ) \)
        real(real32) :: cmp_k !! \( { k }^{ \prime } := \sqrt{ 1 - { k }^{ 2 } } \)
        real(real32) :: nom_k !! nome \( q \) for elliptic integrals corresponding to modulus \( k \)
        real(real32) :: tmp



        if ( ieee_is_nan(k) ) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

            return

        end if



        abs_k = abs(k)



        if (abs_k .gt. 1.0_real32) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

        else if (abs_k .lt. 1.0_real32) then

            cmp_k = sqrt( (1.0_real32 - k) * (1.0_real32 + k) )

            if ( (k * k) .lt. 0.5_real32 ) then

                agm_k    = arithmetic_geometric_mean_kernel(1.0_real32, cmp_k)
                integral = half_pi_real32 / agm_k

            else

                agm_k    = arithmetic_geometric_mean_kernel(1.0_real32, k)
                tmp      = half_pi_real32 / agm_k
                nom_k    = elliptic_nome_auto(cmp_k)
                integral = mns_inv_pi_real32 * tmp * log(nom_k)

            end if

        else

            integral = ieee_value( x = k, class = ieee_positive_inf )

        end if

    end function elliptic_k_real32



    elemental function elliptic_k_real64(k) result(integral)
        !! Compute the complete elliptic integral of the first kind \( K(k) \) using AGM
        !! 
        !! @warning
        !! - The elliptic modulus \( k \) must satisfy \( |k| \le 1 \)
        !! - Returns `ieee_quiet_nan`    for \( |k| > 1 \)
        !! - Returns `ieee_positive_inf` for \( |k| = 1 \)
        !! @endwarning
        !! 
        !! @note
        !! Dependencies:
        !! [`arithmetic_geometric_mean_kernel`](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/interface/arithmetic_geometric_mean_kernel.html),
        !! [`elliptic_nome_auto`](https://dscf-1224.github.io/elliptic_nome_fortran/interface/elliptic_nome_auto.html)
        !! @endnote

        real(real64), intent(in) :: k !! elliptic modulus \( k \)



        real(real64) :: integral !! \( K(k) \)



        real(real64) :: abs_k
        real(real64) :: agm_k !! \( \text{AGM}( 1, { k }^{ \prime } ) \)
        real(real64) :: cmp_k !! \( { k }^{ \prime } := \sqrt{ 1 - { k }^{ 2 } } \)
        real(real64) :: nom_k !! nome \( q \) for elliptic integrals corresponding to modulus \( k \)
        real(real64) :: tmp



        if ( ieee_is_nan(k) ) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

            return

        end if



        abs_k = abs(k)



        if (abs_k .gt. 1.0_real64) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

        else if (abs_k .lt. 1.0_real64) then

            cmp_k = sqrt( (1.0_real64 - k) * (1.0_real64 + k) )

            if ( (k * k) .lt. 0.5_real64 ) then

                agm_k    = arithmetic_geometric_mean_kernel(1.0_real64, cmp_k)
                integral = half_pi_real64 / agm_k

            else

                agm_k    = arithmetic_geometric_mean_kernel(1.0_real64, k)
                tmp      = half_pi_real64 / agm_k
                nom_k    = elliptic_nome_auto(cmp_k)
                integral = mns_inv_pi_real64 * tmp * log(nom_k)

            end if

        else

            integral = ieee_value( x = k, class = ieee_positive_inf )

        end if

    end function elliptic_k_real64



    elemental function elliptic_k_real128(k) result(integral)
        !! Compute the complete elliptic integral of the first kind \( K(k) \) using AGM
        !! 
        !! @warning
        !! - The elliptic modulus \( k \) must satisfy \( |k| \le 1 \)
        !! - Returns `ieee_quiet_nan`    for \( |k| > 1 \)
        !! - Returns `ieee_positive_inf` for \( |k| = 1 \)
        !! @endwarning
        !! 
        !! @note
        !! Dependencies:
        !! [`arithmetic_geometric_mean_kernel`](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/interface/arithmetic_geometric_mean_kernel.html),
        !! [`elliptic_nome_auto`](https://dscf-1224.github.io/elliptic_nome_fortran/interface/elliptic_nome_auto.html)
        !! @endnote

        real(real128), intent(in) :: k !! elliptic modulus \( k \)



        real(real128) :: integral !! \( K(k) \)



        real(real128) :: abs_k
        real(real128) :: agm_k !! \( \text{AGM}( 1, { k }^{ \prime } ) \)
        real(real128) :: cmp_k !! \( { k }^{ \prime } := \sqrt{ 1 - { k }^{ 2 } } \)
        real(real128) :: nom_k !! nome \( q \) for elliptic integrals corresponding to modulus \( k \)
        real(real128) :: tmp



        if ( ieee_is_nan(k) ) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

            return

        end if



        abs_k = abs(k)



        if (abs_k .gt. 1.0_real128) then

            integral = ieee_value( x = k, class = ieee_quiet_nan )

        else if (abs_k .lt. 1.0_real128) then

            cmp_k = sqrt( (1.0_real128 - k) * (1.0_real128 + k) )

            if ( (k * k) .lt. 0.5_real128 ) then

                agm_k    = arithmetic_geometric_mean_kernel(1.0_real128, cmp_k)
                integral = half_pi_real128 / agm_k

            else

                agm_k    = arithmetic_geometric_mean_kernel(1.0_real128, k)
                tmp      = half_pi_real128 / agm_k
                nom_k    = elliptic_nome_auto(cmp_k)
                integral = mns_inv_pi_real128 * tmp * log(nom_k)

            end if

        else

            integral = ieee_value( x = k, class = ieee_positive_inf )

        end if

    end function elliptic_k_real128

end module elliptic_k_using_agm_fortran
