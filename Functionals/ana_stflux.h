      SUBROUTINE ana_stflux (ng, tile, model, itrc)
!
!! svn $Id: ana_stflux.h 751 2015-01-07 22:56:36Z arango $
!!======================================================================
!! Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets kinematic surface flux of tracer type variables   !
!  "stflx" (tracer units m/s) using analytical expressions.            !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_grid
      USE mod_forces
      USE mod_ncparam
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model, itrc

#include "tile.h"
!
      CALL ana_stflux_tile (ng, tile, model, itrc,                      &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      IminS, ImaxS, JminS, JmaxS,                 &
#ifdef SHORTWAVE
     &                      FORCES(ng) % srflx,                         &
#endif
#ifdef CHANNEL2
     &                      GRID(ng) % yr,                              &
#endif
#ifdef TL_IOMS
     &                      FORCES(ng) % tl_stflx,                      &
#endif
     &                      FORCES(ng) % stflx)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME(31)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_stflux
!
!***********************************************************************
      SUBROUTINE ana_stflux_tile (ng, tile, model, itrc,                &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
#ifdef SHORTWAVE
     &                            srflx,                                &
#endif
#ifdef CHANNEL2
     &                             yr,                                  &
#endif
#ifdef TL_IOMS
     &                            tl_stflx,                             &
#endif
     &                            stflx)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
      USE exchange_2d_mod, ONLY : exchange_r2d_tile
#ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
#endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model, itrc
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
#ifdef ASSUMED_SHAPE
# ifdef SHORTWAVE
      real(r8), intent(in) :: srflx(LBi:,LBj:)
# endif
#ifdef CHANNEL2
      real(r8), intent(in) :: yr(LBi:,LBj:) 
#endif
      real(r8), intent(inout) :: stflx(LBi:,LBj:,:)
# ifdef TL_IOMS
      real(r8), intent(inout) :: tl_stflx(LBi:,LBj:,:)
# endif
#else
# ifdef SHORTWAVE
      real(r8), intent(in) :: srflx(LBi:UBi,LBj:UBj)
# endif
#ifdef CHANNEL2
      real(r8), intent(in) :: yr(LBi:UBi,LBj:UBj) 
#endif
      real(r8), intent(inout) :: stflx(LBi:UBi,LBj:UBj,NT(ng))
# ifdef TL_IOMS
      real(r8), intent(inout) :: tl_stflx(LBi:UBi,LBj:UBj,NT(ng))
# endif
#endif
!
!  Local variable declarations.
!
      integer :: i, j
#ifdef DIURNAL_CYCLE_DAN
      real(r8) :: srflxbar
#endif
#ifdef FRONTAL_ZONE
      real(r8) :: CBCZ, Rigeo, dTdz0
#endif
#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Set kinematic surface heat flux (degC m/s) at horizontal
!  RHO-points.
!-----------------------------------------------------------------------
!
      IF (itrc.eq.itemp) THEN
        DO j=JstrT,JendT
          DO i=IstrT,IendT
#ifdef BL_TEST
            stflx(i,j,itrc)=srflx(i,j)
# ifdef TL_IOMS
            tl_stflx(i,j,itrc)=srflx(i,j)
# endif
#elif defined CHANNEL2
        IF (yr(i,j).lt.(5.0_r8/6.0_r8*el(ng))) THEN 
            stflx(i,j,itrc)=-10.0_r8/(rho0*cp)*                         &
     &                      COS(3.0_r8*pi*yr(i,j)/el(ng))
        ELSE
            stflx(i,j,itrc)=0.0_r8
        END IF
# ifdef TL_IOMS
        IF (yr(i,j).lt.(5.0_r8/6.0_r8*el(ng)) THEN 
            tl_stflx(i,j,itrc)=-10.0_r8/(rho0*cp)*                      &
     &                      COS(3.0_r8*pi*yr(i,j)/el(ng))
        ELSE
            tl_stflx(i,j,itrc)=0.0_r8
        END IF
# endif
#else
!            stflx(i,j,itrc)=srflx(i,j)-tdays(ng)-dstart                 &
!     &         -250.0_r8COS(pi*(tdays(ng)-dstart)/365.0_r8)**2.0_r8
#ifdef DIURNAL_CYCLE_DAN
            srflxbar=158.075_r8/(rho0*cp)
            stflx(i,j,itrc)=srflx(i,j)-srflxbar
#else
# ifdef FRONTAL_ZONE
! WT17 case:
            stflx(i,j,itrc)=-0.000001799_r8
!            stflx(i,j,itrc)=0.0_r8
!            Rigeo = REAL(Rigeo_sbl(ng),r8)
!            CBCZ = (coriolis(ng)**2.0_r8)*9.8_r8*Tcoef(ng)
!            dTdz0 = (Rigeo*(Ssqr(ng)**2.0_r8))/CBCZ
! FIXED AT THE GRADIENT:
!            stflx(i,j,itrc)=0.00005_r8*dTdz0
! OW CASE: DIURNAL CYCLE 300 W/m2 pm
!            stflx(i,j,itrc)=-0.000072_r8*COS(2.0_r8*pi*(tdays(ng)-dstart))
#else
            stflx(i,j,itrc)=0.0_r8
#endif
#endif
# ifdef TL_IOMS
            tl_stflx(i,j,itrc)=0.0_r8
# endif
#endif
          END DO
        END DO
!
!-----------------------------------------------------------------------
!  Set kinematic surface freshwater flux (m/s) at horizontal
!  RHO-points, scaling by surface salinity is done in STEP3D.
!-----------------------------------------------------------------------
!
      ELSE IF (itrc.eq.isalt) THEN
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            stflx(i,j,itrc)=0.0_r8
#ifdef TL_IOMS
            tl_stflx(i,j,itrc)=0.0_r8
#endif
          END DO
        END DO
!
!-----------------------------------------------------------------------
!  Set kinematic surface flux (T m/s) of passive tracers, if any.
!-----------------------------------------------------------------------
!
      ELSE
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            stflx(i,j,itrc)=0.0_r8
#ifdef TL_IOMS
            tl_stflx(i,j,itrc)=0.0_r8
#endif
          END DO
        END DO
      END IF
!
!  Exchange boundary data.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          stflx(:,:,itrc))
#ifdef TL_IOMS
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          tl_stflx(:,:,itrc))
#endif
      END IF

#ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, model, 1,                           &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    stflx(:,:,itrc))
# ifdef TL_IOMS
      CALL mp_exchange2d (ng, tile, model, 1,                           &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    tl_stflx(:,:,itrc))
# endif
#endif

      RETURN
      END SUBROUTINE ana_stflux_tile
