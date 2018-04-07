      SUBROUTINE ana_btflux (ng, tile, model, itrc)
!
!! svn $Id: ana_btflux.h 751 2015-01-07 22:56:36Z arango $
!!======================================================================
!! Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets kinematic bottom flux of tracer type variables    !
!  (tracer units m/s).                                                 !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_forces
      USE mod_ncparam
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model, itrc

#include "tile.h"
!
      CALL ana_btflux_tile (ng, tile, model, itrc,                      &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      IminS, ImaxS, JminS, JmaxS,                 &
#ifdef TL_IOMS
     &                      FORCES(ng) % tl_btflx,                      &
#endif
     &                      FORCES(ng) % btflx)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME( 3)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_btflux
!
!***********************************************************************
      SUBROUTINE ana_btflux_tile (ng, tile, model, itrc,                &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
#ifdef TL_IOMS
     &                            tl_btflx,                             &
#endif
     &                            btflx)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model, itrc
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
#ifdef ASSUMED_SHAPE
      real(r8), intent(inout) :: btflx(LBi:,LBj:,:)
# ifdef TL_IOMS
      real(r8), intent(inout) :: tl_btflx(LBi:,LBj:,:)
# endif
#else
      real(r8), intent(inout) :: btflx(LBi:UBi,LBj:UBj,NT(ng))
# ifdef TL_IOMS
      real(r8), intent(inout) :: tl_btflx(LBi:UBi,LBj:UBj,NT(ng))
# endif
#endif
!
!  Local variable declarations.
!
      integer :: i, j
#ifdef FRONTAL_ZONE
      real(r8) :: CBCZ, Rigeo, dTdz0
#endif

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Set kinematic bottom heat flux (degC m/s) at horizontal RHO-points.
!-----------------------------------------------------------------------
!
      IF (itrc.eq.itemp) THEN
        DO j=JstrT,JendT
          DO i=IstrT,IendT
#ifdef FRONTAL_ZONE
            Rigeo = REAL(Rigeo_deep(ng),r8)
            CBCZ = (coriolis(ng)**2.0_r8)*9.8_r8*Tcoef(ng)
            dTdz0 = (Rigeo*(Ssqr(ng)**2.0_r8))/CBCZ
            btflx(i,j,itrc)=0.00005_r8*dTdz0
#else
            btflx(i,j,itrc)=0.0_r8
#endif
#ifdef TL_IOMS
            tl_btflx(i,j,itrc)=0.0_r8
#endif
          END DO
        END DO
!
!-----------------------------------------------------------------------
!  Set kinematic bottom salt flux (m/s) at horizontal RHO-points,
!  scaling by bottom salinity is done elsewhere.
!-----------------------------------------------------------------------
!
      ELSE IF (itrc.eq.isalt) THEN
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            btflx(i,j,itrc)=0.0_r8
#ifdef TL_IOMS
            tl_btflx(i,j,itrc)=0.0_r8
#endif
          END DO
        END DO
!
!-----------------------------------------------------------------------
!  Set kinematic bottom flux (T m/s) of passive tracers, if any.
!-----------------------------------------------------------------------
!
      ELSE
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            btflx(i,j,itrc)=0.0_r8
#ifdef TL_IOMS
            tl_btflx(i,j,itrc)=0.0_r8
#endif
          END DO
        END DO
      END IF

      RETURN
      END SUBROUTINE ana_btflux_tile