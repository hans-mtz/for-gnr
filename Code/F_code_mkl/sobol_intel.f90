include 'mkl_vsl.f90'

module intel_sobol

    USE MKL_VSL_TYPE
    USE MKL_VSL

    implicit none

    public :: isobol

    private

contains

    subroutine isobol(r,seed)
        !This subrouting is a wrapper for the sobol low discrepancy random
        !numbers from intel. It generates a sobol random number between 0 and 1.
        !Input is the seed.
        !Output is r is the vector of the random numbers to be generated
        !
        real(kind=8) r(:,:) ! buffer for random numbers
        ! real(kind=8) s        ! average
        real(kind=8) a, b
        ! integer(4), INTENT(IN) :: v ! parameters of uniform distribution

        ! CHARACTER(3) name="stream"

        ! name=name//"1"
        integer(kind=4) errcode
        integer(kind=4) i,s,j,k
        integer brng,method,n
        integer, INTENT(IN) :: seed

        TYPE (VSL_STREAM_STATE) :: stream1



        n = size(r(:,1))
        ! s = 0.0
        a = 0.0d0
        b = 1.0d0
        s = seed

        ! sigma  = 2.0
        brng=VSL_BRNG_SOBOL
        method=VSL_RNG_METHOD_UNIFORM_STD_ACCURATE
        ! seed=777

        !     ***** Initializing *****
        errcode=vslnewstream( stream(1), brng,  s )

        ! Creating streams
        streamloop: do i=2,size(stream)
            errcode=vslcopystream(stream(i), stream(1))
        enddo streamloop

        !Leapgrogging the streams
        leap: do j=1,size(stream)
            errcode = vslleapfrogstream(stream(i), i-1, size(stream))
        enddo leap

        !     ***** Generating *****
        rng: do k = 1,size(stream)
            errcode=vdrnguniform( method, stream(k), n, r(:,k), a, b )
        !     do j = 1, 1000
        !         s = s + r(j)
        !     end do
        enddo rng

        ! s = s / 10000.0

        !     ***** Deinitialize *****
        delete: do i=1,size(stream)
            errcode=vsldeletestream( stream(i) )
        enddo delete

        !     ***** Printing results *****
        ! print *,"Sample mean of normal distribution = ", s
    end subroutine isobol



end module intel_sobol




! program MKL_VSL_GAUSSIAN




! end
