#include "cppdefs.h"
      MODULE uBCcalc_mod
#if defined NONLINEAR && defined BC_ZONE
! calculate the geostrophic shear velocity uBC and ubarBC
      implicit none
      PRIVATE
      PUBLIC  ::  uBCcalc

      CONTAINS

      SUBROUTINE uBCcalc (ng,tile)
      USE mod_grid
      USE mod_param
      USE mod_ocean
      USE mod_stepping
      USE mod_coupling
#if defined DIAGNOSTICS_UV
      USE mod_diags
#endif
!
      integer, intent(in) :: ng, tile

#include "tile.h"

#ifdef PROFILE
      CALL wclock_on (ng, iNLM, 21)
#endif
      CALL uBCcalc_tile (ng, tile,                                      &
     &                   LBi, UBi, LBj, UBj,                            &
     &                              IminS, ImaxS, JminS, JmaxS,         &
     &                   OCEAN(ng) % uBC,                               &
     &                   OCEAN(ng) % ubarBC,                            &
     &                            GRID(ng) % h,                         &
     &                            OCEAN(ng) % zeta,                     &
     &                            GRID(ng) % yr,                        &
     &                            GRID(ng) % Hz,                        &
     &                            GRID(ng) % z_r)
#ifdef PROFILE
      CALL wclock_off (ng, iNLM, 21)
#endif
      RETURN
      END SUBROUTINE uBCcalc
!
      SUBROUTINE uBCcalc_tile (ng, tile,                                &
     &                         LBi, UBi, LBj, UBj,                      &
     &                              IminS, ImaxS, JminS, JmaxS,         &
     &                         uBC, ubarBC, h, zeta, yr, Hz, z_r) 

      USE mod_param
      USE mod_scalars
      USE exchange_3d_mod
      USE exchange_2d_mod
# ifdef DISTRIBUTE
      USE mp_exchange_mod
# endif
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
#ifdef ASSUMED_SHAPE
      real(r8), intent(inout) :: uBC(LBi:,LBj:,:)
      real(r8), intent(inout) :: ubarBC(LBi:,LBj:)
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: zeta(LBi:,LBj:,:)
      real(r8), intent(in) :: yr(LBi:,LBj:)
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
#else
      real(r8), intent(out) :: uBC(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(out) :: ubarBC(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: zeta(LBi:UBi,LBj:UBj,3)
      real(r8), intent(in) :: yr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
#endif
! Local var
      integer :: i, j, k
      real(r8) :: Dnow, cff4, cff5

#include "set_bounds.h"

      DO k=1,N(ng)
        DO j=Jstr,Jend
          DO i=IstrU,Iend
        Dnow = h(i,j)+h(i-1,j)-zeta(i,j,1)-zeta(i-1,j,1);
        uBC(i,j,k) = Ssqr(ng)/coriolis(ng)*                             &
     &        (1.0_r8-TANH((yr(i,j)-yc_bcz(ng))/Lf_bcz(ng))**2.0_r8)*   &
     &   0.25_r8*(Dnow+2.0_r8*(z_r(i,j,k)+z_r(i-1,j,k)))
          END DO
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          Dnow = h(i,j)+h(i-1,j)+zeta(i,j,1)+zeta(i-1,j,1)
          cff5=0.5_r8*(Hz(i-1,j,1)+Hz(i,j,1))
          ubarBC(i,j)=cff5*uBC(i,j,1)
          DO k=2,N(ng)
            cff5=0.5_r8*(Hz(i-1,j,k)+Hz(i,j,k))
            ubarBC(i,j)=ubarBC(i,j)+cff5*uBC(i,j,k)
          END DO
          cff4=2.0_r8/Dnow
          ubarBC(i,j)=ubarBC(i,j)*cff4
        END DO
      END DO
      DO k=1,N(ng)
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            uBC(i,j,k) = uBC(i,j,k)-ubarBC(i,j)
          END DO
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          ubarBC(i,j)=0.0_r8
        END DO
      END DO
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          uBC)
        CALL exchange_u2d_tile (ng,tile,                                &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          ubarBC)
      END IF
!
# ifdef DISTRIBUTE
      CALL mp_exchange3d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    uBC)
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    ubarBC)
# endif
      RETURN
      END SUBROUTINE uBCcalc_tile
#endif
      END MODULE uBCcalc_mod
