program check_reference0

    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: elliptic_k_using_agm_fortran



    implicit none



    character(*), parameter :: reference_file = 'test/reference0.csv'



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine close_file(unit, stat, msg)

        integer, intent(in) :: unit

        integer, intent(out) :: stat

        character(*), intent(inout) :: msg



        close( &!
        unit   = unit , &!
        iostat = stat , &!
        iomsg  = msg    &!
        )

        call handle_stat(stat, msg)

    end subroutine close_file



    subroutine handle_stat(stat, msg)

        integer, intent(in) :: stat

        character(*), intent(in) :: msg



        if (stat .ne. 0) then

            write( unit = error_unit, fmt = '(A,I0)') 'iostat : ', stat
            write( unit = error_unit, fmt = '(A,A)' ) 'iomsg  : ', trim(msg)

            error stop

        end if

    end subroutine handle_stat



    subroutine open_reference_file(unit, stat, msg)

        integer, intent(inout) :: unit

        integer, intent(out) :: stat

        character(*), intent(inout) :: msg



        open( &!
        file    = reference_file , &!
        newunit = unit           , &!
        action  = 'read'         , &!
        status  = 'old'          , &!
        iostat  = stat           , &!
        iomsg   = msg              &!
        )

        call handle_stat(stat, msg)

    end subroutine open_reference_file



    subroutine test_real32

        logical :: flag_error_stop

        integer :: file_unit

        integer :: line

        integer :: stat

        real(real32) :: elliptic_k_cal

        real(real32) :: elliptic_k_dif

        real(real32) :: elliptic_k_ref

        real(real32) :: k

        real(real32) :: ulp_error

        real(real32) :: ulp_error_abs

        character(256) :: msg



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )



        flag_error_stop = .false.
        line            = 0



        do

            read( &!
            unit   = file_unit , &!
            fmt    = *         , &!
            iostat = stat      , &!
            iomsg  = msg         &!
            ) &!
            k, elliptic_k_ref

            if ( is_iostat_end(stat) ) then
                exit
            else
                call handle_stat(stat, msg)
            end if



            line           = line + 1

            elliptic_k_cal = elliptic_k(k)
            elliptic_k_dif = elliptic_k_cal -         elliptic_k_ref
            ulp_error      = elliptic_k_dif / spacing(elliptic_k_ref)
            ulp_error_abs  = abs(ulp_error)



            if ( ulp_error_abs .gt. 2.0_real32 ) then

                if ( ulp_error_abs .gt. 10.0_real32 ) then

                    write( unit = error_unit, fmt = * ) &!
                        'FAIL: Unacceptable error'

                    flag_error_stop = .true.

                else

                    write( unit = error_unit, fmt = * ) &!
                        'WARNING: Large ULP error'

                end if



                write( unit = error_unit, fmt = * ) &!
                    'k               = ' , k

                write( unit = error_unit, fmt = * ) &!
                    'line            = ' , line

                write( unit = error_unit, fmt = * ) &!
                    'elliptic_k(ref) = ' ,  elliptic_k_ref

                write( unit = error_unit, fmt = * ) &!
                    'elliptic_k(cal) = ' ,  elliptic_k_cal

                write( unit = error_unit, fmt = * ) &!
                    'ULP error       = ' ,  ulp_error



                if (flag_error_stop) error stop

            end if

        end do



        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_real32



    subroutine test_real64

        logical :: flag_error_stop

        integer :: file_unit

        integer :: line

        integer :: stat

        real(real64) :: elliptic_k_cal

        real(real64) :: elliptic_k_dif

        real(real64) :: elliptic_k_ref

        real(real64) :: k

        real(real64) :: ulp_error

        real(real64) :: ulp_error_abs

        character(256) :: msg



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )



        flag_error_stop = .false.
        line            = 0



        do

            read( &!
            unit   = file_unit , &!
            fmt    = *         , &!
            iostat = stat      , &!
            iomsg  = msg         &!
            ) &!
            k, elliptic_k_ref

            if ( is_iostat_end(stat) ) then
                exit
            else
                call handle_stat(stat, msg)
            end if



            line           = line + 1

            elliptic_k_cal = elliptic_k(k)
            elliptic_k_dif = elliptic_k_cal -         elliptic_k_ref
            ulp_error      = elliptic_k_dif / spacing(elliptic_k_ref)
            ulp_error_abs  = abs(ulp_error)



            if ( ulp_error_abs .gt. 2.0_real64 ) then

                if ( ulp_error_abs .gt. 10.0_real64 ) then

                    write( unit = error_unit, fmt = * ) &!
                        'FAIL: Unacceptable error'

                    flag_error_stop = .true.

                else

                    write( unit = error_unit, fmt = * ) &!
                        'WARNING: Large ULP error'

                end if



                write( unit = error_unit, fmt = * ) &!
                    'k               = ' , k

                write( unit = error_unit, fmt = * ) &!
                    'line            = ' , line

                write( unit = error_unit, fmt = * ) &!
                    'elliptic_k(ref) = ' ,  elliptic_k_ref

                write( unit = error_unit, fmt = * ) &!
                    'elliptic_k(cal) = ' ,  elliptic_k_cal

                write( unit = error_unit, fmt = * ) &!
                    'ULP error       = ' ,  ulp_error



                if (flag_error_stop) error stop

            end if

        end do



        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_real64



    subroutine test_real128

        logical :: flag_error_stop

        integer :: file_unit

        integer :: line

        integer :: stat

        real(real128) :: elliptic_k_cal

        real(real128) :: elliptic_k_dif

        real(real128) :: elliptic_k_ref

        real(real128) :: k

        real(real128) :: ulp_error

        real(real128) :: ulp_error_abs

        character(256) :: msg



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )



        flag_error_stop = .false.
        line            = 0



        do

            read( &!
            unit   = file_unit , &!
            fmt    = *         , &!
            iostat = stat      , &!
            iomsg  = msg         &!
            ) &!
            k, elliptic_k_ref

            if ( is_iostat_end(stat) ) then
                exit
            else
                call handle_stat(stat, msg)
            end if



            line           = line + 1

            elliptic_k_cal = elliptic_k(k)
            elliptic_k_dif = elliptic_k_cal -         elliptic_k_ref
            ulp_error      = elliptic_k_dif / spacing(elliptic_k_ref)
            ulp_error_abs  = abs(ulp_error)



            if ( ulp_error_abs .gt. 2.0_real128 ) then

                if ( ulp_error_abs .gt. 10.0_real128 ) then

                    write( unit = error_unit, fmt = * ) &!
                        'FAIL: Unacceptable error'

                    flag_error_stop = .true.

                else

                    write( unit = error_unit, fmt = * ) &!
                        'WARNING: Large ULP error'

                end if



                write( unit = error_unit, fmt = * ) &!
                    'k               = ' , k

                write( unit = error_unit, fmt = * ) &!
                    'line            = ' , line

                write( unit = error_unit, fmt = * ) &!
                    'elliptic_k(ref) = ' ,  elliptic_k_ref

                write( unit = error_unit, fmt = * ) &!
                    'elliptic_k(cal) = ' ,  elliptic_k_cal

                write( unit = error_unit, fmt = * ) &!
                    'ULP error       = ' ,  ulp_error



                if (flag_error_stop) error stop

            end if

        end do



        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_real128

end program check_reference0
