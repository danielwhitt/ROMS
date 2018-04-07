      SUBROUTINE ana_passive (ng, tile, model)
!
!! svn $Id: ana_passive.h 751 2015-01-07 22:56:36Z arango $
!!======================================================================
!! Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets initial conditions for passive inert tracers      !
!  using analytical expressions.                                       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_ncparam
      USE mod_ocean
      USE mod_grid
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      CALL ana_passive_tile (ng, tile, model,                           &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
#ifdef FRONTAL_ZONE
     &                       GRID(ng) % xr, GRID(ng) % yr,              &
     &                       GRID(ng) % z_r,                            &
#endif
     &                       OCEAN(ng) % t)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME(18)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_passive
!
!***********************************************************************
      SUBROUTINE ana_passive_tile (ng, tile, model,                     &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
#ifdef FRONTAL_ZONE
     &                               xr, yr,                            &
     &                                  z_r,                            &
#endif
     &                             t)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
#ifdef ASSUMED_SHAPE
#ifdef FRONTAL_ZONE 
      real(r8), intent(in) :: xr(LBi:,LBj:)
      real(r8), intent(in) :: yr(LBi:,LBj:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
#endif
      real(r8), intent(out) :: t(LBi:,LBj:,:,:,:)
#else
#ifdef FRONTAL_ZONE 
      real(r8), intent(in) :: xr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: yr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
#endif
      real(r8), intent(out) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#endif
!
!  Local variable declarations.
!
      integer :: i, ip, itrc, j, k
#ifdef FRONTAL_ZONE 
      real(r8) :: x, y, Rigeo, Rigeo1, dTdz0, CBCZ, dTdz1
#endif
!
#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Set analytical initial conditions for passive inert tracers.
!-----------------------------------------------------------------------
!
#if defined GEOSTROPHIC3D 
      DO ip=1,NPT
        itrc=inert(ip)
        DO k=1,N(ng)
          DO j=JstrT,JendT
            DO i=IstrT,IendT
              IF (k.gt.41.0_r8) THEN
!              IF (k.gt.164.0_r8) THEN
!              IF (k.gt.328.0_r8) THEN
              t(i,j,k,1,itrc)=1.0_r8
              ELSE
              t(i,j,k,1,itrc)=0.0_r8
              END IF
              t(i,j,k,2,itrc)=t(i,j,k,1,itrc)
            END DO
          END DO
        END DO
      END DO
#elif defined PACSUBM 
      DO ip=1,NPT
        itrc=inert(ip)
        DO k=1,N(ng)
          DO j=JstrT,JendT
            DO i=IstrT,IendT
              t(i,j,k,1,itrc)=0.0_r8
              t(i,j,k,2,itrc)=t(i,j,k,1,itrc)
            END DO
          END DO
        END DO
      END DO
#elif defined FRONTAL_ZONE
!      Rigeo =  REAL(Rigeo_sbl(ng),r8)
!      Rigeo1 = REAL(Rigeo_deep(ng),r8)
!      CBCZ = (coriolis(ng)**2.0_r8)*9.8_r8*Tcoef(ng)
!      dTdz0 = (Rigeo*(Ssqr(ng)**2.0_r8))/CBCZ
      dTdz1 = 0.0667_r8 
      DO ip=1,NPT
         itrc=inert(ip)
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            IF (ABS(z_r(i,j,k)).gt.depth_pyc(ng)) THEN
         t(i,j,k,1,itrc)=2.0_r8
!        t(i,j,k,1,itrc)=1.5_r8*T0(ng)+dTdz1*(z_r(i,j,k)+depth_sbl(ng))  &
!     &       +0.00001_r8*RAND(0)
!          x=0.5_r8*(xr(i,j-1)+xr(i,j))
!        t(i,j,k,1,itrc)=t(i,j,k,1,itrc)+                                &
!     &    0.8_r8*COS(6.2832_r8*x/(50.0_r8*REAL(Lm(ng),r8)))*            &
!     &        (depth_sbl(ng)+z_r(i,j,k))/depth_sbl(ng)
            ELSEIF ((ABS(z_r(i,j,k)).lt.depth_pyc(ng)).AND.             &
     &              (ABS(z_r(i,j,k)).gt.depth_sbl(ng))) THEN
            t(i,j,k,1,itrc)=2.0_r8-1.9_r8*(z_r(i,j,k)+depth_pyc(ng))/   &
     &                                   (depth_pyc(ng)-depth_sbl(ng))
!            t(i,j,k,1,itrc)=1.5_r8*T0(ng)                               &
!     &                       +dTdz0*(z_r(i,j,k)+depth_sbl(ng))          &
!     &                       +0.00001_r8*RAND(0)
!          x=0.5_r8*(xr(i,j-1)+xr(i,j))
!        t(i,j,k,1,itrc)=t(i,j,k,1,itrc)+                                &
!     &    0.2_r8*COS(6.2832_r8*x/(50.0_r8*REAL(Lm(ng),r8)))*            &
!     &        (depth_sbl(ng)+z_r(i,j,k))/depth_sbl(ng)
            ELSE
         t(i,j,k,1,itrc)=0.1_r8
            END IF
          END DO
        END DO
      END DO
      END DO
#else
      ana_passive.h: no values provided for t(:,:,:,1,inert(itrc))
#endif

      RETURN
      END SUBROUTINE ana_passive_tile
