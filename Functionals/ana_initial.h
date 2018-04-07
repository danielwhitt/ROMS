      SUBROUTINE ana_initial (ng, tile, model)
!
!! svn $Id: ana_initial.h 751 2015-01-07 22:56:36Z arango $
!!======================================================================
!! Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine sets initial conditions for momentum and tracer     !
!  type variables using analytical expressions.                        !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
#ifdef FRONTAL_ZONE
      USE IFPORT
#endif
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      IF (model.eq.iNLM) THEN
        CALL ana_NLMinitial_tile (ng, tile, model,                      &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
     &                            GRID(ng) % h,                         &
#ifdef SPHERICAL
     &                            GRID(ng) % lonr,                      &
     &                            GRID(ng) % latr,                      &
#else
     &                            GRID(ng) % xr,                        &
     &                            GRID(ng) % yr,                        &
#endif
#ifdef SOLVE3D
     &                            GRID(ng) % z_r,                       &
     &                            OCEAN(ng) % u,                        &
     &                            OCEAN(ng) % v,                        &
     &                            OCEAN(ng) % t,                        &
#endif
     &                            OCEAN(ng) % ubar,                     &
     &                            OCEAN(ng) % vbar,                     &
     &                            OCEAN(ng) % zeta)
#ifdef TANGENT
      ELSE IF ((model.eq.iTLM).or.(model.eq.iRPM)) THEN
        CALL ana_TLMinitial_tile (ng, tile, model,                      &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
     &                            kstp(ng),                             &
# ifdef SOLVE3D
     &                            nstp(ng),                             &
     &                            OCEAN(ng) % tl_u,                     &
     &                            OCEAN(ng) % tl_v,                     &
     &                            OCEAN(ng) % tl_t,                     &
# endif
     &                            OCEAN(ng) % tl_ubar,                  &
     &                            OCEAN(ng) % tl_vbar,                  &
     &                            OCEAN(ng) % tl_zeta)
#endif
#ifdef ADJOINT
      ELSE IF (model.eq.iADM) THEN
        CALL ana_ADMinitial_tile (ng, tile, model,                      &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
     &                            knew(ng),                             &
# ifdef SOLVE3D
     &                            nstp(ng),                             &
     &                            OCEAN(ng) % ad_u,                     &
     &                            OCEAN(ng) % ad_v,                     &
     &                            OCEAN(ng) % ad_t,                     &
# endif
     &                            OCEAN(ng) % ad_ubar,                  &
     &                            OCEAN(ng) % ad_vbar,                  &
     &                            OCEAN(ng) % ad_zeta)
#endif
      END IF
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME(10)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_initial
!
!***********************************************************************
      SUBROUTINE ana_NLMinitial_tile (ng, tile, model,                  &
     &                                LBi, UBi, LBj, UBj,               &
     &                                IminS, ImaxS, JminS, JmaxS,       &
     &                                h,                                &
#ifdef SPHERICAL
     &                                lonr, latr,                       &
#else
     &                                xr, yr,                           &
#endif
#ifdef SOLVE3D
     &                                z_r,                              &
     &                                u, v, t,                          &
#endif
     &                                ubar, vbar, zeta)
!***********************************************************************
!
      USE mod_param
      USE mod_grid
      USE mod_scalars
#ifdef FRONTAL_ZONE
      USE IFPORT
#endif

#ifdef CHANNEL
!
# ifdef DISTRIBUTE
      USE distribute_mod, ONLY : mp_bcasti
# endif
      USE erf_mod, ONLY : ERF
#endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
#ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: h(LBi:,LBj:)
# ifdef SPHERICAL
      real(r8), intent(in) :: lonr(LBi:,LBj:)
      real(r8), intent(in) :: latr(LBi:,LBj:)
# else
      real(r8), intent(in) :: xr(LBi:,LBj:)
      real(r8), intent(in) :: yr(LBi:,LBj:)
# endif
# ifdef SOLVE3D
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)

      real(r8), intent(out) :: u(LBi:,LBj:,:,:)
      real(r8), intent(out) :: v(LBi:,LBj:,:,:)
      real(r8), intent(out) :: t(LBi:,LBj:,:,:,:)
# endif
      real(r8), intent(out) :: ubar(LBi:,LBj:,:)
      real(r8), intent(out) :: vbar(LBi:,LBj:,:)
      real(r8), intent(out) :: zeta(LBi:,LBj:,:)
#else
# ifdef SPHERICAL
      real(r8), intent(in) :: lonr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: latr(LBi:UBi,LBj:UBj)
# else
      real(r8), intent(in) :: xr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: yr(LBi:UBi,LBj:UBj)
# endif
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
# ifdef SOLVE3D
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))

      real(r8), intent(out) :: u(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(out) :: v(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(out) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
# endif
      real(r8), intent(out) :: ubar(LBi:UBi,LBj:UBj,3)
      real(r8), intent(out) :: vbar(LBi:UBi,LBj:UBj,3)
      real(r8), intent(out) :: zeta(LBi:UBi,LBj:UBj,3)
#endif
!
!  Local variable declarations.
!
      integer :: Iless, Iplus, i, itrc, j, k

#ifdef CHANNEL
      real(r8), parameter :: guscale = 40.0E+03_r8
      real(r8), parameter :: u0 = 1.6_r8
#endif
#ifdef FRONTAL_ZONE 
      integer :: rndseed
      real(r8) :: Rigeo, Rigeo1, dTdz0, CBCZ, dTdz1, dTdz2, Rigeo2
      real(r8) :: Lx, Ly, randph3, randph4, wnum, a
#endif

      real(r8) :: depth, dx, val1, val2, val3, val4, x, x0, y, y0

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Initial conditions for 2D momentum (m/s) components.
!-----------------------------------------------------------------------
!
#if defined CHANNEL && !defined ONLY_TS_IC
      y0=0.5_r8*el(ng)
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          val1=(GRID(ng)%yu(i,j)-y0)/guscale
          ubar(i,j,1)=u0*EXP(-val1*val1)/6.0_r8
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          vbar(i,j,1)=0.0_r8
        END DO
      END DO
#elif defined SOLITON
      x0=2.0_r8*xl(ng)/3.0_r8
      y0=0.5_r8*el(ng)
      val1=0.395_r8
      val2=0.771_r8*(val1*val1)
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          x=0.5_r8*(xr(i-1,j)+xr(i,j))-x0
          y=0.5_r8*(yr(i-1,j)+yr(i,j))-y0
          val3=EXP(-val1*x)
          val4=val2*((2.0_r8*val3/(1.0_r8+(val3*val3)))**2)
          ubar(i,j,1)=0.25_r8*val4*(6.0_r8*y*y-9.0_r8)*                 &
     &                EXP(-0.5_r8*y*y)
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          x=0.5_r8*(xr(i,j-1)+xr(i,j))-x0
          y=0.5_r8*(yr(i,j-1)+yr(i,j))-y0
          val3=EXP(-val1*x)
          val4=val2*((2.0_r8*val3/(1.0_r8+(val3*val3)))**2)
          vbar(i,j,1)=2.0_r8*val4*y*(-2.0_r8*val1*TANH(val1*x))*        &
     &                EXP(-0.5_r8*y*y)
        END DO
      END DO
#elif defined CASCADE
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          x=0.5_r8*(xr(i-1,j)+xr(i,j))
          y=0.5_r8*(yr(i-1,j)+yr(i,j))
         ubar(i,j,1)=-9.81_r8*Tcoef(ng)/6.0_r8/0.0001_r8*               &
     &     dTdy0(ng)*COS(kg0(ng)*y)+uabar0(ng)
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          x=0.5_r8*(xr(i,j-1)+xr(i,j))
          y=0.5_r8*(yr(i,j-1)+yr(i,j))
          vbar(i,j,1)=vabar0(ng)
        END DO
      END DO
#elif defined FRONTAL_ZONE
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          x=0.5_r8*(xr(i-1,j)+xr(i,j))
          y=0.5_r8*(yr(i-1,j)+yr(i,j))
         ubar(i,j,1)=0.0_r8
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          x=0.5_r8*(xr(i,j-1)+xr(i,j))
          y=0.5_r8*(yr(i,j-1)+yr(i,j))
          vbar(i,j,1)=0.0_r8
!     &                      *depth_sbl(ng)/depth_bcz(ng)*               &
!     &    COS(6.2832_r8*y/(50.0_r8*REAL(Mm(ng),r8)))
        END DO
      END DO
#elif defined RIVERPLUME2
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          ubar(i,j,1)=0.0_r8
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          vbar(i,j,1)=-0.05_r8
        END DO
      END DO
#elif defined SED_TEST1
      val1=100.0_r8
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          ubar(i,j,1)=-10.0_r8/(10.0_r8+9.0E-06_r8*REAL(i,r8)*val1)
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          vbar(i,j,1)=0.0_r8
        END DO
      END DO
#elif defined SED_TOY
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          ubar(i,j,1)=0.0_r8
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          vbar(i,j,1)=0.0_r8
        END DO
      END DO
#elif defined TEST_CHAN
      val1=100.0_r8
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          ubar(i,j,1)=0.0_r8
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          vbar(i,j,1)=0.0_r8
        END DO
      END DO
#else
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          ubar(i,j,1)=0.0_r8
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          vbar(i,j,1)=0.0_r8
        END DO
      END DO
#endif
!
!-----------------------------------------------------------------------
!  Initial conditions for free-surface (m).
!-----------------------------------------------------------------------
!
#if defined CHANNEL && !defined ONLY_TS_IC
      y0=0.5_r8*el(ng)
# ifdef SOLVE3D
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          val1=(yr(i,j)-y0)/guscale
          val2=-u0*guscale*GRID(ng)%f(i,j)*SQRT(pi)/(12.0_r8*g)
          zeta(i,j,1)=val2*ERF(val1)
        END DO
      END DO
# else
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          val1=(yr(i,j)-y0))/guscale
          val2=-0.5_r8*u0*guscale*GRID(ng)%f(i,j)*sqrt(pi)/g
          zeta(i,j,1)=val2*ERF(val1)
        END DO
      END DO
# endif
# ifdef DISTRIBUTE
      CALL mp_bcasti (ng, model, exit_flag)  ! in case of error in ERF
# endif
#elif defined KELVIN
!!    val1=1.0_r8                               ! zeta0
!!    val2=2.0_r8*pi/(12.42_r8*3600.0_r8)       ! M2 Tide period
      DO j=JstrT,JendT
        DO i=IstrT,IendT
!!        zeta(i,j,1)=val1*                                             &
!!   &                EXP(-GRID(ng)%f(i,j)*GRID(ng)%yp(i,j)/            &
!!   &                    SQRT(g*GRID(ng)%h(i,j)))*                     &
!!   &                COS(val2*GRID(ng)%xp(i,j)/                        &
!!   &                    SQRT(g*GRID(ng)%h(i,j)))
          zeta(i,j,1)=0.0_r8
        END DO
      END DO
#elif defined SOLITON
      x0=2.0_r8*xl(ng)/3.0_r8
      y0=0.5_r8*el(ng)
      val1=0.395_r8
      val2=0.771_r8*(val1*val1)
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          x=xr(i,j)-x0
          y=yr(i,j)-y0
          val3=EXP(-val1*x)
          val4=val2*((2.0_r8*val3/(1.0_r8+(val3*val3)))**2)
          zeta(i,j,1)=0.25_r8*val4*(6.0_r8*y*y+3.0_r8)*                 &
     &                EXP(-0.5_r8*y*y)
        END DO
      END DO
#elif defined CASCADE
      DO j=JstrT,JendT
        DO i=IstrT,IendT
         zeta(i,j,1)=0.5_r8*Tcoef(ng)/kg0(ng)*dTdy0(ng)                 &
     &   *SIN(kg0(ng)*yr(i,j))*h(i,j)
        END DO
      END DO
#elif defined SED_TEST1
      val1=100.0_r8
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          zeta(i,j,1)=9.0E-06_r8*REAL(i,r8)*val1
        END DO
      END DO
#elif defined TEST_CHAN
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          zeta(i,j,1)=-0.4040_r8*REAL(i,r8)/REAL(Lm(ng)+1,r8)
        END DO
      END DO
#else
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          zeta(i,j,1)=0.0_r8
        END DO
      END DO
#endif

#ifdef SOLVE3D
!
!-----------------------------------------------------------------------
!  Initial conditions for 3D momentum components (m/s).
!-----------------------------------------------------------------------
!
# if defined CHANNEL && !defined ONLY_TS_IC
      y0=0.5_r8*el(ng)
      DO k=1,N(ng)
        DO j=JstrT,JendT
         DO i=IstrP,IendT
           val1=(GRID(ng)%yu(i,j)-y0)/guscale
           val2=(z_r(i,j,k)+z_r(i-1,j,k))/(h(i,j)+h(i-1,j))
           val3=u0*(0.5_r8+val2+(0.5_r8*val2*val2))*EXP(-val1*val1)
           u(i,j,k,1)=val3
         END DO
        END DO
      END DO
      DO k=1,N(ng)
        DO j=JstrP,JendT
          DO i=IstrT,IendT
            v(i,j,k,1)=0.0_r8
          END DO
        END DO
      END DO
# elif defined RIVERPLUME2
      DO k=1,N(ng)
       DO j=JstrT,JendT
         DO i=IstrP,IendT
            u(i,j,k,1)=0.0_r8
          END DO
        END DO
        DO j=JstrP,JendT
          DO i=IstrT,IendT
            v(i,j,k,1)=-0.05_r8*LOG((h(i,j)+z_r(i,j,k))/Zob(ng))/       &
     &                 (LOG(h(i,j)/Zob(ng))-1.0_r8+Zob(ng)/h(i,j))
          END DO
        END DO
      END DO
# elif defined SED_TEST1
      DO k=1,N(ng)
       DO j=JstrT,JendT
         DO i=IstrP,IendT
            u(i,j,k,1)=-1.0_r8*LOG((h(i,j)+z_r(i,j,k))/Zob(ng))/        &
     &                 (LOG(h(i,j)/Zob(ng))-1.0_r8+Zob(ng)/h(i,j))
          END DO
        END DO
        DO j=JstrP,JendT
          DO i=IstrT,IendT
            v(i,j,k,1)=0.0_r8
          END DO
        END DO
      END DO
# elif defined SED_TOY
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrP,IendT
            u(i,j,k,1)=1.0_r8
!!          u(i,j,k,1)=-1.0_r8
!!          u(i,j,k,1)=0.0_r8
          END DO
        END DO
        DO j=JstrP,JendT
          DO i=IstrT,IendT
            v(i,j,k,1)=0.0_r8
          END DO
        END DO
      END DO
# elif defined CASCADE
      Do k=1,N(ng)
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          x=0.5_r8*(xr(i-1,j)+xr(i,j))
          y=0.5_r8*(yr(i-1,j)+yr(i,j))
       u(i,j,k,1)=-9.81_r8*Tcoef(ng)/0.0001_r8*dTdy0(ng)               &
     &            *COS(kg0(ng)*y)*                                     &
     &      ((z_r(i,j,k)+h(i,j))**2.0_r8)/(2.0_r8*h(i,j))
        IF (z_r(i,j,k).gt.(-40.0_r8)) THEN
       u(i,j,k,1)=u(i,j,k,1)+uabar0(ng)*h(i,j)/40.0_r8
        END IF
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          x=0.5_r8*(xr(i,j-1)+xr(i,j))
          y=0.5_r8*(yr(i,j-1)+yr(i,j))
        IF (z_r(i,j,k).gt.(-40.0_r8)) THEN
       v(i,j,k,1)=vabar0(ng)*h(i,j)/40.0_r8
        ELSE
       v(i,j,k,1)=0.0_r8
        END IF
      END DO
      END DO
      END DO
# elif defined FRONTAL_ZONE
      rndseed = seedgen(tile)
      CALL SRAND(rndseed)
      Do k=1,N(ng)
      DO j=JstrT,JendT
        DO i=IstrP,IendT
!        IF (z_r(i,j,k).gt.(-depth_pyc(ng))) THEN
       u(i,j,k,1)= RAND(0)*0.0001_r8-0.00005_r8
       Lx=xl(ng)
       Ly=el(ng)
       x=0.5_r8*(xr(i-1,j)+xr(i,j))
       y=yr(i,j)
       a=0.001_r8
       wnum=1.0_r8
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.796_r8))
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.099_r8))
       wnum=2.0_r8
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.262_r8))
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Lx+0.335_r8))
       wnum=3.0_r8
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.680_r8))
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.137_r8))
       wnum=4.0_r8
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.721_r8))
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.107_r8))
       wnum=5.0_r8
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.654_r8))
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.494_r8))
       wnum=6.0_r8
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.779_r8))
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.715_r8))
       wnum=7.0_r8
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.904_r8))
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.891_r8))
       wnum=8.0_r8
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.334_r8))
       u(i,j,k,1)=u(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.699_r8))
! multiply by tanh 
       u(i,j,k,1)=u(i,j,k,1)*(0.5_r8*TANH((z_r(i,j,k)+40.0_r8)/10.0_r8)  &
     &            +0.5_r8)
!        ELSE
!       u(i,j,k,1)= 0.0_r8
!        END IF 
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
!        IF (z_r(i,j,k).gt.(-depth_pyc(ng))) THEN
          y=0.5_r8*(yr(i,j-1)+yr(i,j))
!        v(i,j,k,1)=0.0_r8*                                              &
!       v(i,j,k,1)=0.06667_r8*depth_bcz(ng)/depth_sbl(ng)*               &
!     &    COS(6.2832_r8*y/(50.0_r8*REAL(Mm(ng),r8)))
        v(i,j,k,1)=RAND(0)*0.0001_r8-0.00005_r8
       Lx=xl(ng)
       Ly=el(ng)
       x=xr(i,j)
       a=0.001_r8
       wnum=1.0_r8
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.796_r8))
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.099_r8))
       wnum=2.0_r8
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.262_r8))
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Lx+0.335_r8))
       wnum=3.0_r8
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.680_r8))
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.137_r8))
       wnum=4.0_r8
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.721_r8))
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.107_r8))
       wnum=5.0_r8
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.654_r8))
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.494_r8))
       wnum=6.0_r8
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.779_r8))
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.715_r8))
       wnum=7.0_r8
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.904_r8))
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.891_r8))
       wnum=8.0_r8
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*x/Lx+0.334_r8))
       v(i,j,k,1)=v(i,j,k,1)+a/wnum*SIN(2.0_r8*pi*(wnum*y/Ly+0.699_r8))
! multiply by tanh 
       v(i,j,k,1)=v(i,j,k,1)*(0.5_r8*TANH((z_r(i,j,k)+40.0_r8)/10.0_r8)  &
     &            +0.5_r8)
!        ELSE
!          y=0.5_r8*(yr(i,j-1)+yr(i,j))
!       v(i,j,k,1)=-0.06667_r8*depth_bcz(ng)/                            &
!     &                               (depth_bcz(ng)-depth_sbl(ng))*     &
!        v(i,j,k,1)=0.0_r8
!     &    COS(6.2832_r8*y/(50.0_r8*REAL(Mm(ng),r8)))
!        END IF
      END DO
      END DO
      END DO
# else
      DO k=1,N(ng)
       DO j=JstrT,JendT
         DO i=IstrP,IendT
            u(i,j,k,1)=0.0_r8
          END DO
        END DO
        DO j=JstrP,JendT
          DO i=IstrT,IendT
            v(i,j,k,1)=0.0_r8
          END DO
        END DO
      END DO
# endif
!
!-----------------------------------------------------------------------
!  Initial conditions for tracer type variables.
!-----------------------------------------------------------------------
!
!  Set initial conditions for potential temperature (Celsius) and
!  salinity (PSU).
!
# if defined BENCHMARK
      val1=(44.69_r8/39.382_r8)**2
      val2=val1*(rho0*800.0_r8/g)*(5.0E-05_r8/((42.689_r8/44.69_r8)**2))
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=val2*EXP(z_r(i,j,k)/800.0_r8)*             &
     &                       (0.6_r8-0.4_r8*TANH(z_r(i,j,k)/800.0_r8))
            t(i,j,k,1,isalt)=35.0_r8
          END DO
        END DO
      END DO
# elif defined BASIN
      val1=(44.69_r8/39.382_r8)**2
      val2=val1*(rho0*800.0_r8/g)*(5.0E-05_r8/((42.689_r8/44.69_r8)**2))
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=val2*EXP(z_r(i,j,k)/800.0_r8)*             &
     &                       (0.6_r8-0.4_r8*TANH(z_r(i,j,k)/800.0_r8))
          END DO
        END DO
      END DO
# elif defined BL_TEST
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            val1=TANH(1.1_r8*z_r(i,j,k)+11.0_r8)
            t(i,j,k,1,itemp)=T0(ng)+6.25_r8*val1
            t(i,j,k,1,isalt)=S0(ng)-0.75_r8*val1
          END DO
        END DO
      END DO
# elif defined CHANNEL
      y0=0.5_r8*el(ng)
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            val1=(yr(i,j)-y0)/guscale
            val2=-0.5_r8*u0*guscale*GRID(ng)%f(i,j)*SQRT(pi)/            &
     &           (Tcoef(ng)*g*h(i,j))
            val3=(val2*ERF(val1)+T0(ng))*(1.0_r8+z_r(i,j,k)/h(i,j))
            t(i,j,k,1,itemp)=val3
          END DO
        END DO
      END DO
#  ifdef DISTRIBUTE
      CALL mp_bcasti (ng, model, exit_flag)  ! in case of error in ERF
#  endif
# elif defined CANYON
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=3.488_r8*EXP(z_r(i,j,k)/800.0_r8)*         &
     &                       (1.0_r8-(2.0_r8/3.0_r8)*                   &
     &                               TANH(z_r(i,j,k)/800.0_r8))
          END DO
        END DO
      END DO
# elif defined CHANNEL_NECK
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=20.0_r8
!!          t(i,j,k,1,itemp)=14.0_r8+8.0_r8*EXP(z_r(i,j,k)/50.0_r8)
          END DO
        END DO
      END DO
# elif defined COUPLING_TEST
      val1=40.0_r8
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=val1*EXP(z_r(i,j,k)/800.0_r8)*             &
     &                       (0.6_r8-0.4_r8*TANH(z_r(i,j,k)/800.0_r8))+ &
     &                       1.5_r8
            t(i,j,k,1,isalt)=35.0_r8
          END DO
        END DO
      END DO
# elif defined DOUBLE_GYRE
      val1=(44.69_r8/39.382_r8)**2
      val2=val1*(rho0*100.0_r8/g)*(5.0E-5_r8/((42.689_r8/44.69_r8)**2))
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            val3=T0(ng)+val2*EXP(z_r(i,j,k)/100.0_r8)*                  &
     &           (10.0_r8-0.4_r8*TANH(z_r(i,j,k)/100.0_r8))
            val4=yr(i,j)/el(ng)
            t(i,j,k,1,itemp)=val3-3.0_r8*val4
#  ifdef SALINITY
            t(i,j,k,1,isalt)=34.5_r8-0.001_r8*z_r(i,j,k)-val4
#  endif
          END DO
        END DO
      END DO
# elif defined ESTUARY_TEST
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=10.0_r8
            IF (xr(i,j).le.30000.0_r8) then
              t(i,j,k,1,isalt)=30.0_r8
            ELSEIF (xr(i,j).le.80000.0_r8) then
              t(i,j,k,1,isalt)=(80000.0_r8-xr(i,j))/50000.0_r8*30.0_r8
            ELSE
              t(i,j,k,1,isalt)=0.0_r8
            END IF
          END DO
        END DO
      END DO
# elif defined FLT_TEST
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=T0(ng)
          END DO
        END DO
      END DO
# elif defined GRAV_ADJ
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,MIN((Lm(ng)+1)/2,IendT)
            t(i,j,k,1,itemp)=T0(ng)+5.0_r8
            t(i,j,k,1,isalt)=0.0_r8
          END DO
          DO i=MAX((Lm(ng)+1)/2+1,IstrT),IendT
            t(i,j,k,1,itemp)=T0(ng)
            t(i,j,k,1,isalt)=S0(ng)
          END DO
!!        DO i=IstrT,IendT
!!          IF (i.lt.Lm(ng)/2) THEN
!!            t(i,j,k,1,itemp)=T0(ng)+5.0_r8
!!          ELSE IF (i.eq.Lm(ng)/2) THEN
!!            t(i,j,k,1,itemp)=T0(ng)+4.0_r8
!!          ELSE IF (i.eq.Lm(ng)/2+1) THEN
!!            t(i,j,k,1,itemp)=T0(ng)+1.0_r8
!!          ELSE
!!            t(i,j,k,1,itemp)=T0(ng)
!!          END IF
!!        END DO
        END DO
      END DO
# elif defined LAB_CANYON
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=-659.34183_r8*z_r(i,j,k)
          END DO
        END DO
      END DO
# elif defined LAKE_SIGNELL
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=10.0_r8
            t(i,j,k,1,isalt)=30.0_r8
          END DO
        END DO
      END DO
# elif defined LMD_TEST
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=MIN(13.0_r8,                               &
     &                           7.0_r8+0.2_r8*(z_r(i,j,k)+50.0_r8))
            t(i,j,k,1,isalt)=35.0_r8
          END DO
        END DO
      END DO
#  elif defined MIXED_LAYER
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=10.0_r8+3.0_r8*(z_r(i,j,k)+h(i,j))/        &
     &                       h(i,j)
            t(i,j,k,1,isalt)=S0(ng)
          END DO
        END DO
      END DO
# elif defined NJ_BIGHT
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            depth=z_r(i,j,k)
            IF (depth.ge.-15.0_r8) THEN
              t(i,j,k,1,itemp)= 2.049264257728403E+01_r8-depth*         &
     &                         (2.640850848793918E-01_r8+depth*         &
     &                         (2.751125328535212E-01_r8+depth*         &
     &                         (9.207489761648872E-02_r8+depth*         &
     &                         (1.449075725742839E-02_r8+depth*         &
     &                         (1.078215685912076E-03_r8+depth*         &
     &                         (3.240318053903974E-05_r8+               &
     &                          1.262826857690271E-07_r8*depth))))))
              t(i,j,k,1,isalt)= 3.066489149193135E+01_r8-depth*         &
     &                         (1.476725262946735E-01_r8+depth*         &
     &                         (1.126455760313399E-01_r8+depth*         &
     &                         (3.900923281871022E-02_r8+depth*         &
     &                         (6.939014937447098E-03_r8+depth*         &
     &                         (6.604436696792939E-04_r8+depth*         &
     &                         (3.191792361954220E-05_r8+               &
     &                          6.177352634409320E-07_r8*depth))))))
            ELSE
               t(i,j,k,1,itemp)=14.6_r8+                                &
     &                          6.70_r8*TANH(1.1_r8*depth+15.9_r8)
               t(i,j,k,1,isalt)=31.3_r8-                                &
     &                          0.55_r8*TANH(1.1_r8*depth+15.9_r8)
            END IF
          END DO
        END DO
      END DO
# elif defined BIOCOLUMN 
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
!            IF (z_r(i,j,k).gt.-50.0_r8) THEN
!            t(i,j,k,1,itemp)=22.0_r8+0.012_r8*z_r(i,j,k)
!            t(i,j,k,1,isalt)=0.0_r8
!            ELSEIF ((z_r(i,j,k).le.-50.0_r8).and.                        &
!     &         (z_r(i,j,k).gt.-300.0_r8)) THEN
!            t(i,j,k,1,itemp)=21.4_r8+0.06_r8*(50.0_r8+z_r(i,j,k))
!            t(i,j,k,1,isalt)=0.0_r8
!            ELSEIF (z_r(i,j,k).le.-300.0_r8) THEN
!            t(i,j,k,1,itemp)=6.4_r8+0.002_r8*(300.0_r8+z_r(i,j,k))
!            t(i,j,k,1,isalt)=0.0_r8
!            END IF
             t(i,j,k,1,itemp)=22.0_r8+0.012_r8*z_r(i,j,k)
             t(i,j,k,1,isalt)=0.0_r8
          END DO
        END DO
      END DO
# elif defined OVERFLOW
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=T0(ng)-0.5_r8*T0(ng)*(1.0_r8+              &
     &                       TANH((yr(i,j)-60000.0_r8)/2000.0_r8))
          END DO
        END DO
      END DO
# elif defined RIVERPLUME1
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=T0(ng)+0.01_r8*REAL(k,r8)
            t(i,j,k,1,isalt)=S0(ng)
          END DO
        END DO
      END DO
# elif defined RIVERPLUME2
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=T0(ng)
            t(i,j,k,1,isalt)=S0(ng)
          END DO
        END DO
      END DO
# elif defined SEAMOUNT
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=T0(ng)+7.5_r8*EXP(z_r(i,j,k)/1000.0_r8)
          END DO
        END DO
      END DO
# elif defined SED_TEST1
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=20.0_r8
            t(i,j,k,1,isalt)=0.0_r8
          END DO
        END DO
      END DO
# elif defined UPWELLING
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=T0(ng)+8.0_r8*EXP(z_r(i,j,k)/50.0_r8)
!!          t(i,j,k,1,itemp)=T0(ng)+(z_r(i,j,k)+75.0_r8)/150.0_r8+
!!   &                       4.0_r8*(1.0_r8+TANH((z_r(i,j,k)+35.0_r8)/
!!   &                                           6.5_r8))
!!          t(i,j,k,1,isalt)=1.0E-04_r8*yr(i,j)-S0(ng)
            t(i,j,k,1,isalt)=S0(ng)
!!          IF (j.lt.Mm(ng)/2) THEN
!!            t(i,j,k,1,isalt)=0.0_r8
!!          ELSE IF (j.eq.Mm(ng)/2) THEN
!!            t(i,j,k,1,isalt)=0.5_r8
!!          ELSE IF (j.gt.Mm(ng)/2) THEN
!!            t(i,j,k,1,isalt)=1.0_r8
!!          END IF
          END DO
        END DO
      END DO
# elif defined CASCADE
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=T0(ng)+dTdz0(ng)*(z_r(i,j,k)+h(i,j))       &
     & +dTdy0(ng)/kg0(ng)/h(i,j)*(z_r(i,j,k)+h(i,j))                    &
     & *SIN(kg0(ng)*yr(i,j))+0.03_r8*RAND(0)-0.015_r8+                  & 
     & 0.02_r8*SIN(2.0_r8*kg0(ng)*xr(i,j))*SIN(pi*z_r(i,j,k)/h(i,j))
!     & +0.01_r8*(z_r(i,j,k)-0.2_r8*h(i,j)*LOG(COSH(5.0_r8*z_r(i,j,k)    &
!     &  /h(i,j)+2.0_r8))+1.461866_r8*h(i,j)) 
! ADD THE TWO LINES ABOVE TO SWITCH TO ML scenario (weak surface N2) 
            t(i,j,k,1,isalt)=0.0_r8
          END DO
        END DO
      END DO
# elif defined FRONTAL_ZONE
      rndseed = seedgen(tile)
      CALL SRAND(rndseed)
      Rigeo =  REAL(Rigeo_sbl(ng),r8)
      Rigeo1 = REAL(Rigeo_deep(ng),r8) 
      Rigeo2 = REAL(Rigeo_pyc(ng),r8) 
      CBCZ = (coriolis(ng)**2.0_r8)*9.8_r8*Tcoef(ng)
      dTdz0 = (Rigeo*(Ssqr(ng)**2.0_r8))/CBCZ
      dTdz1 = (Rigeo1*(Ssqr(ng)**2.0_r8))/CBCZ
      dTdz2 = (Rigeo2*(Ssqr(ng)**2.0_r8))/CBCZ
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
! TANH N2, WT17
           IF (z_r(i,j,k).lt.(-50.0_r8)) THEN
           t(i,j,k,1,itemp)=dTdz1*(z_r(i,j,k)+80.0_r8)/2.0_r8-          &
     & 1.5_r8*dTdz1*LOG(COSH((z_r(i,j,k)+60.0_r8)/3.0_r8))+             &
     & dTdz2*(z_r(i,j,k)+80.0_r8)/2.0_r8+                               &
     & 1.5_r8*dTdz2*LOG(COSH((z_r(i,j,k)+60.0d0)/3.0d0))+               &
     & 1.5_r8*dTdz1*LOG(COSH((-20.0_r8)/3.0_r8))-                       &
     & 1.5_r8*dTdz2*LOG(COSH((-20.0_r8)/3.0_r8))
           ELSE
           t(i,j,k,1,itemp)=1.5_r8*(dTdz0-dTdz2)*                       &
     & LOG(COSH((-z_r(i,j,k)-40.0_r8)/3.0_r8))+0.5_r8*(dTdz2+           &
     & dTdz0)*(z_r(i,j,k)+80.0_r8)-                                     &
     & 1.5_r8*(dTdz0-dTdz2)*                                            &
     & LOG(COSH((10.0_r8)/3.0_r8))-0.5_r8*(dTdz2 +                      &
     & dTdz0)*30.0_r8+                                                  &
     & 15.0_r8*(dTdz1+dTdz2)+                                           &
     & 1.5_r8*(dTdz1-dTdz2)*                                            &
     & LOG(COSH((20.0_r8)/3.0_r8)/COSH(10.0_r8/3.0_r8)) 
           END IF
!            IF (ABS(z_r(i,j,k)).gt.depth_pyc(ng)) THEN
!        t(i,j,k,1,itemp)=dTdz1*(z_r(i,j,k)+depth_bcz(ng))               &
!     &       +dTdz1*RAND(0)*0.1_r8
!            ELSEIF ((ABS(z_r(i,j,k)).gt.depth_sbl(ng)).AND.                 &
!     &          (ABS(z_r(i,j,k)).lt.depth_pyc(ng))) THEN
!        t(i,j,k,1,itemp)=dTdz1*(depth_bcz(ng)-depth_pyc(ng))+           &
!     &                   dTdz2*(z_r(i,j,k)+depth_pyc(ng))               &
!     &       +dTdz2*RAND(0)*0.1_r8
!          x=0.5_r8*(xr(i,j-1)+xr(i,j))
!            ELSE
!            t(i,j,k,1,itemp)= dTdz1*(depth_bcz(ng)-depth_pyc(ng))+      &
!     &                        dTdz2*(depth_pyc(ng)-depth_sbl(ng))+      &
!     &                        dTdz0*(z_r(i,j,k)+depth_sbl(ng))+         &
!     &                        dTdz0*RAND(0)*0.1_r8
!          x=0.5_r8*(xr(i,j-1)+xr(i,j))
!            END IF
            t(i,j,k,1,isalt)=0.0_r8
          END DO
        END DO
      END DO
# elif defined WINDBASIN
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=20.0_r8                ! homogeneous
!!          t(i,j,k,1,itemp)=14.0_r8+8.0_r8*EXP(z_r(i,j,k)/50.0_r8)-    &
!!   &                       T0(ng)                 ! stratified
          END DO
        END DO
      END DO
# elif defined INTERNALTIDE
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
          t(i,j,k,1,itemp)=22.0_r8+0.01_r8*(z_r(i,j,k))
          END DO
        END DO
      END DO
# else
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,itemp)=T0(ng)
#  ifdef SALINITY
            t(i,j,k,1,isalt)=S0(ng)
#  endif
          END DO
        END DO
      END DO
# endif
#endif
      RETURN
      END SUBROUTINE ana_NLMinitial_tile

#ifdef TANGENT
!
!***********************************************************************
      SUBROUTINE ana_TLMinitial_tile (ng, tile, model,                  &
     &                                LBi, UBi, LBj, UBj,               &
     &                                IminS, ImaxS, JminS, JmaxS,       &
     &                                kstp,                             &
# ifdef SOLVE3D
     &                                nstp,                             &
     &                                tl_u, tl_v, tl_t,                 &
# endif
     &                                tl_ubar, tl_vbar, tl_zeta)
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
      integer, intent(in) :: kstp
# ifdef SOLVE3D
      integer, intent(in) :: nstp
# endif
!
# ifdef ASSUMED_SHAPE
#  ifdef SOLVE3D
      real(r8), intent(out) :: tl_u(LBi:,LBj:,:,:)
      real(r8), intent(out) :: tl_v(LBi:,LBj:,:,:)
      real(r8), intent(out) :: tl_t(LBi:,LBj:,:,:,:)
#  endif
      real(r8), intent(out) :: tl_ubar(LBi:,LBj:,:)
      real(r8), intent(out) :: tl_vbar(LBi:,LBj:,:)
      real(r8), intent(out) :: tl_zeta(LBi:,LBj:,:)
# else
#  ifdef SOLVE3D
      real(r8), intent(out) :: tl_u(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(out) :: tl_v(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(out) :: tl_t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#  endif
      real(r8), intent(out) :: tl_ubar(LBi:UBi,LBj:UBj,3)
      real(r8), intent(out) :: tl_vbar(LBi:UBi,LBj:UBj,3)
      real(r8), intent(out) :: tl_zeta(LBi:UBi,LBj:UBj,3)
# endif
!
!  Local variable declarations.
!
      integer :: i, itrc, j, k

# include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Initial conditions for tangent linear 2D momentum (s/m) components.
!-----------------------------------------------------------------------
!
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          tl_ubar(i,j,kstp)=0.0_r8
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          tl_vbar(i,j,kstp)=0.0_r8
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Initial conditions for tangent linear free-surface (1/m).
!-----------------------------------------------------------------------
!
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          tl_zeta(i,j,kstp)=0.0_r8
        END DO
      END DO
# ifdef SOLVE3D
!
!-----------------------------------------------------------------------
!  Initial conditions for tangent linear 3D momentum components (s/m).
!-----------------------------------------------------------------------
!
      DO k=1,N(ng)
       DO j=JstrT,JendT
         DO i=IstrP,IendT
            tl_u(i,j,k,nstp)=0.0_r8
          END DO
        END DO
        DO j=JstrP,JendT
          DO i=IstrT,IendT
            tl_v(i,j,k,nstp)=0.0_r8
          END DO
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Initial conditions for tangent linear active tracers (1/Tunits).
!-----------------------------------------------------------------------
!
      DO itrc=1,NAT
        DO k=1,N(ng)
          DO j=JstrT,JendT
            DO i=IstrT,IendT
              tl_t(i,j,k,nstp,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END DO
# endif
      RETURN
      END SUBROUTINE ana_TLMinitial_tile
#endif

#ifdef ADJOINT
!
!***********************************************************************
      SUBROUTINE ana_ADMinitial_tile (ng, tile, model,                  &
     &                                LBi, UBi, LBj, UBj,               &
     &                                IminS, ImaxS, JminS, JmaxS,       &
     &                                knew,                             &
# ifdef SOLVE3D
     &                                nstp,                             &
     &                                ad_u, ad_v, ad_t,                 &
# endif
     &                                ad_ubar, ad_vbar, ad_zeta)
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
      integer, intent(in) :: knew
# ifdef SOLVE3D
      integer, intent(in) :: nstp
# endif
!
# ifdef ASSUMED_SHAPE
#  ifdef SOLVE3D
      real(r8), intent(out) :: ad_u(LBi:,LBj:,:,:)
      real(r8), intent(out) :: ad_v(LBi:,LBj:,:,:)
      real(r8), intent(out) :: ad_t(LBi:,LBj:,:,:,:)
#  endif
      real(r8), intent(out) :: ad_ubar(LBi:,LBj:,:)
      real(r8), intent(out) :: ad_vbar(LBi:,LBj:,:)
      real(r8), intent(out) :: ad_zeta(LBi:,LBj:,:)
# else
#  ifdef SOLVE3D
      real(r8), intent(out) :: ad_u(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(out) :: ad_v(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(out) :: ad_t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#  endif
      real(r8), intent(out) :: ad_ubar(LBi:UBi,LBj:UBj,3)
      real(r8), intent(out) :: ad_vbar(LBi:UBi,LBj:UBj,3)
      real(r8), intent(out) :: ad_zeta(LBi:UBi,LBj:UBj,3)
# endif
!
!  Local variable declarations.
!
      integer :: i, itrc, j, k

# include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Initial conditions for adjoint 2D momentum (s/m) components.
!-----------------------------------------------------------------------
!
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          ad_ubar(i,j,knew)=0.0_r8
        END DO
      END DO
      DO j=JstrP,JendT
        DO i=IstrT,IendT
          ad_vbar(i,j,knew)=0.0_r8
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Initial conditions for adjoint free-surface (1/m).
!-----------------------------------------------------------------------
!
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          ad_zeta(i,j,knew)=0.0_r8
        END DO
      END DO
# ifdef SOLVE3D
!
!-----------------------------------------------------------------------
!  Initial conditions for adjoint 3D momentum components (s/m).
!-----------------------------------------------------------------------
!
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrP,IendT
            ad_u(i,j,k,nstp)=0.0_r8
          END DO
        END DO
        DO j=JstrP,JendT
          DO i=IstrT,IendT
            ad_v(i,j,k,nstp)=0.0_r8
          END DO
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Initial conditions for adjoint active tracers (1/Tunits).
!-----------------------------------------------------------------------
!
      DO itrc=1,NAT
        DO k=1,N(ng)
          DO j=JstrT,JendT
            DO i=IstrT,IendT
              ad_t(i,j,k,nstp,itrc)=0.0_r8
            END DO
          END DO
        END DO
      END DO
# endif
      RETURN
      END SUBROUTINE ana_ADMinitial_tile
#endif
#ifdef FRONTAL_ZONE
      FUNCTION seedgen(pid)
        use iso_fortran_env
        implicit none
        integer :: seedgen
        integer, intent(in) :: pid
        integer :: s

        call system_clock(s)
        seedgen = abs( mod((s*181)*((pid-83)*359), 104729) )
      END FUNCTION seedgen
#endif