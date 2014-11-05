!-------------------------------------------------------------------------------
! Name: test_rttov.F90
!
! Purpose:
! Create environment within all RTTOV related code is contained.
!
! Arguments:
! Name         Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! profiles     struct out  Meteorological profile input to RTTOV
! preproc_dims struct both Summary of preprocessing grid definitions
!
! History:
! 2012/03/27, MJ: provides initial implementation based on the example program
!   example_fw of Annex X of the RTTOV V10.2 user guide V1.3.
! 2012/05/23, MJ: fixes bug with AVHRR setup.
! 2012/06/20, CP: removed emissivity. implemented new calls to call_rtm_ir and
!   call_rtm_solar
! 2012/07/29, CP: improved readability added in algorithm description, added in
!   month variable required for emissivity
! 2014/02/10, AP: variable renaming
! 2014/07/10, AP: Removed test_flag argument
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

subroutine test_rttov(profiles,preproc_dims)

   use preproc_constants
   use preproc_structures
   use rttov_types, only : &
        rttov_options,     &
        rttov_coefs,       &
        profile_type,      &
        transmission_type, &
        radiance_type,     &
        radiance_aux,      &
        rttov_chanprof

   implicit none

   type(profile_type),   intent(out), Allocatable :: profiles(:)
   type(preproc_dims_s), intent(inout)            :: preproc_dims

   write(*,*)'size p',size(profiles(1)%p)
   profiles(1)%p=(/ 0.1000000, 0.2899999, 0.6900000, 1.4200000, 2.6110001, &
                    4.4070001, 6.9499998, 10.370000, 14.810000, 20.400000, &
                    27.260000, 35.509998, 45.290001, 56.730000, 69.970001, &
                    85.180000, 102.05000, 122.04000, 143.84000, 167.95000, &
                    194.36000, 222.94000, 253.71001, 286.60001, 321.50000, &
                    358.28000, 396.81000, 436.95001, 478.54001, 521.46002, &
                    565.53998, 610.59998, 656.42999, 702.72998, 749.12000, &
                    795.09003, 839.95001, 882.79999, 922.46002, 957.44000, &
                    985.88000, 1005.4300, 1013.2500 /)

   profiles(1)%t=(/ 233.60001, 241.67704, 253.23011, 252.50557, 246.91315, &
                    239.69681, 233.24844, 228.03089, 223.56791, 219.07674, &
                    216.15725, 215.22403, 215.57992, 216.57970, 217.11310, &
                    217.49276, 217.80880, 217.88191, 217.68036, 217.40253, &
                    217.62805, 218.01734, 219.10488, 220.24099, 224.75238, &
                    229.00494, 233.22402, 237.49662, 241.52803, 245.22418, &
                    248.06919, 250.75688, 253.29428, 255.68380, 257.23785, &
                    259.27726, 261.82443, 264.41749, 266.54156, 268.19030, &
                    269.48697, 270.35680, 270.70001 /)

   profiles(1)%q=(/ 4.0500000, 4.0500001, 4.3824993, 4.5500000, 4.5857745, &
                    4.5495059, 4.3017402, 4.1264096, 4.0225951, 3.9796100, &
                    4.0016360, 3.6613762, 3.3855700, 3.6410484, 3.7614872, &
                    3.5784096, 3.2430191, 3.3512358, 3.7808084, 4.3003834, &
                    4.7790456, 5.7238246, 8.6251679, 15.251495, 142.33706, &
                    262.13141, 631.84424, 1339.8199, 2007.8288, 2689.4818, &
                    3625.0054, 4508.7961, 5343.1642, 6128.9072, 7034.8201, &
                    7486.7355, 7549.5909, 7591.0796, 7610.0000, 7610.0000, &
                    7610.0000, 7610.0000, 7610.0000 /)

   profiles(1)%o3=(/ 0.78399998,  1.1684838,   2.5833953,   3.6549304, &
                     4.1020903,   4.5486539,   4.9972324,   5.3940101, &
                     5.7309561,   5.8995998,   5.6926492,   5.0676270, &
                     4.2179655,   3.2703149,   2.3685558,   1.5558411, &
                     0.89208919,  0.61021386,  0.42071174,  0.26682224,&
                     0.17988501,  0.11061394,  0.095672204, 0.081846168, &
                     0.076923555, 0.072283365, 0.068001149, 0.064092043, &
                     0.060403617, 0.056964043, 0.053972930, 0.051147221, &
                     0.048479528, 0.045967304, 0.043873698, 0.041779328, &
                     0.039809831, 0.038305856, 0.037199071, 0.036461990, &
                     0.035882301, 0.035493435, 0.035340000 /)
   preproc_dims%kdim=43

   !2m/10m variables
   !profiles(1)%s2m%t=
   !              profiles(1)%s2m%p=exp(preproc_prtm%lnsp(idim,jdim))*hpa2pa
   !              profiles(1)%s2m%u=
   !              profiles(1)%s2m%v=

   !skin variables
   !              profiles(1)%skin%t=preproc_prtm%skin_temp(idim,jdim)
   !              !Hardwire surface types
   profiles(1) % skin % surftype  = 0
   profiles(1) % skin % watertype = 1
   !              profiles(1)%elevation=0.0

   !profiles(1)%zenangle=54
   !                 profiles(1)%azangle=0.0
   !                 profiles(1)%sunzenangle=40.
   !                 profiles(1)%sunazangle=121.


   !
   !end of test code
   !

end subroutine test_rttov
