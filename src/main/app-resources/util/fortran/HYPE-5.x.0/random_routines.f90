!> \file random_routines.f90
!> Contains module random_routines.

!> Routines for random number generation, including routines for spatially correlated random fields.
!  Most of the code is written by David Gustafsson and Nils Gustafsson, SMHI, except for fft991 by Clive Temperton, ECMWF.
!  Last update: D.Gustafsson, 20150622

MODULE RANDOM_ROUTINES
!Copyright 2015,2017 SMHI
!
!This file is part of HYPE.
!HYPE is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!HYPE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License for more details.
!You should have received a copy of the Lesser GNU General Public License along with HYPE. If not, see <http://www.gnu.org/licenses/>.
!-----------------------------------------------------------------------------------------

IMPLICIT NONE

!Type declarations
!>Type with data needed to generate spatially correlated random data for a vector of X,Y points
  TYPE randxy_data
    ! vectors corresponding to the points in the (HYSS) model domain (the sub-basin centroids)
    real, allocatable :: pert_xy(:) !<random numbers interpolated from the 2D field
    real, allocatable :: x(:)       !<easting
    real, allocatable :: y(:)       !<northing
    integer           :: nxy        !<number of points
    
    ! variables for the 2D perturbation field
    real, allocatable :: pert_field(:,:) !<spatially correlated 2D pertubation field (nxl,nyl)
    real              :: gridsize        !<grid size in x and y direction
    integer           :: nxl,nyl         !grid dimensions
    real, allocatable :: xfield(:)       !<vector with x coordinates in the 2D field
    real, allocatable :: yfield(:)       !<vector with y coordinates in the 2D field
    
    ! variables for the 2D spectral correlation field
    real, allocatable :: corr(:,:)         !<correlation field, dimensions (nxl,nyl)
    real, allocatable :: spec_dens_2D(:,:) !<spectral density field, dimensions (0:kmax,-lmax:lmax)
    integer           :: kmax         !<dimension (kmax=nxl/2)
    integer           :: lmax         !<dimension (lmax=nyl/2)

    ! parameters defining the spatial correlation
    real              :: lscale        !<correlation length
    integer           :: index_corr    !<correlation type: 1 Gaussian, 2 Compact 5th degree polynomial, 3 Power law

    ! FFT help arrays
    integer           :: ifaxx(13)
    integer           :: ifaxy(13)
    real, allocatable :: trigsx(:)     !(3*nxl/2+1)
    real, allocatable :: trigsy(:)     !(3*nyl/2+1)

  END TYPE randxy_data

CONTAINS
!Contains blocks of:
! random number generation: gaussian, uniform, spatially correlated fields
! spatial interpolation:
! fft:

!-----------------------------------------------------------------------------------------
!RANDOM NUMBER GENERATION
!-----------------------------------------------------------------------------------------
!!> Function for generattion of
!!! pseudo-random number from normal distribution calling the rgauss function
!!-----------------------------------------------------------------------------------------
!  REAL FUNCTION randnorm(mu,sigma)
!   REAL, INTENT(IN) :: mu    !<mean value
!   REAL, INTENT(IN) :: sigma !<standard deviation
!   !generate random number N(0,1) and rescale to N(mu,sigma)
!   randnorm = mu + rgauss()*sigma   
!  END FUNCTION randnorm
!!-----------------------------------------------------------------------------------------
!!> Function for generation of
!!! random number from uniform distribution U[umin,umax]
!!-----------------------------------------------------------------------------------------
!  REAL FUNCTION randunif(umin,umax)
!   REAL, INTENT(IN) :: umin    !<minimum value in range
!   REAL, INTENT(IN) :: umax    !<maximum value in range
!   !generate random number [0,1]
!   call random_number(randunif)
!   !rescale to [umin,umax]
!   randunif = umin + (umax-umin)*randunif   
!  END FUNCTION randunif
!-----------------------------------------------------------------------------------------
!> Function for generation pseudo-random number from normal distribution N(0,1).
! algorithm from Nils Gustafsson, reference?
!-----------------------------------------------------------------------------------------
  REAL FUNCTION rgauss()
    INTEGER nsample
    PARAMETER (nsample=100)
    REAL ran(nsample),sum
    INTEGER i
    !set sum=0
    sum=0.
    !generate uniform random numbers U(0,1)
    call random_number(ran)
    !calculate the sum of the random numbers
    do i=1,nsample
     sum=sum+ran(i)
    end do
    !rescale sum into N(0,1)
    rgauss=(sum-real(nsample)*0.5)/sqrt(real(nsample-1)/12.)
    return
  END FUNCTION rgauss
!------------------------------------------------------------------------------------------
!> Subroutine for creating pseudo-random number (gaussian) with spatial correlation 
!!for point coordinates (x,y)
!!
! 1) FFT-based algorithm for 2D random fields by Nils Gustafsson
! 2) The 2D random numbers are interpolated to the requested x,y coordinates (centroids of
!    the hydrological model subbasins)
! 
!! Please note, the randxy data structure must be initialized by the subroutine init_randxy_data
!! prior to calling the resample routine
!------------------------------------------------------------------------------------------
  SUBROUTINE resample_randxy_data(randxy)
    TYPE(randxy_data), INTENT(INOUT) :: randxy

    !>\b Algorithm
    
    !> Generate a new 2D random field
    call random_field(randxy%nxl, randxy%nyl, randxy%kmax,randxy%lmax, &
                      randxy%spec_dens_2D,randxy%pert_field, &
                      randxy%ifaxx, randxy%ifaxy, &
                      randxy%trigsx, randxy%trigsy)

    !> Interpolate to the requested x,y coordinates
    call interpol_grid2point(randxy%nxy,randxy%x,randxy%y,randxy%pert_xy, &
                           randxy%nxl,randxy%nyl, &
                           randxy%xfield,randxy%yfield,randxy%pert_field)                         
    return
  END SUBROUTINE resample_randxy_data

!------------------------------------------------------------------------------------------
!> Initialisation of data used for generation of spatially correlated random numbers
!! with the subroutine resample_randxy_data.
!------------------------------------------------------------------------------------------
  SUBROUTINE init_randxy_data(randxy,np,xp,yp,lscale,gridsize,index_corr)

    !Input arguments
    TYPE(randxy_data), INTENT(INOUT) :: randxy !<data structure
    INTEGER, INTENT(IN)       :: np            !<n:o spatial points (model units, sub-basins)
    REAL, INTENT(IN)          :: xp(np)        !<easting(x) coordinates
    REAL, INTENT(IN)          :: yp(np)        !<northing(y) coordinates
    REAL, INTENT(IN)          :: lscale        !<correlation length scale
    REAL, INTENT(IN)          :: gridsize      !<gridsize for the 2D field where the random numbers are generated
    INTEGER, INTENT(IN)       :: index_corr    !<correlation type: 1 Gaussian, 2 Compact 5th degree polynomial, 3 Power law

    !local variables
    REAL    :: xmax,xmin,ymax,ymin
    INTEGER :: ip,ix,jy

    ! Identify coordinates of bounding box covering the x and y model points
    xmax = xp(1) ; xmin=xp(1)
    ymax = yp(1) ; ymin=yp(1)
    DO ip=2,np
      IF(xp(ip).GT.xmax)xmax = xp(ip)
      IF(xp(ip).LT.xmin)xmin = xp(ip)
      IF(yp(ip).GT.ymax)ymax = yp(ip)
      IF(yp(ip).LT.ymin)ymin = yp(ip)
    ENDDO

    ! Design a 2D grid covering the x-y boundingbox with enough margin, taking into account the biperiodic boundary
    ! and with appropriate dimensions for the ftt-algorithms  
    !
    !   1) to avoid the effect of the biperiodic boundary 
    !       we need some margin, for instance 4*lscale:
    !
    !       (nxl-1)*gridsize-(xmax-xmin) > 4*lscale
    !       (nyl-1)*gridsize-(ymax-ymin) > 4*lscale
    !
    !   2) fft991 require that nxl and nyl MUST BE AN EVEN NUMBER GREATER THAN 4 THAT HAS
    !      NO OTHER FACTORS EXCEPT POSSIBLY POWERS OF 2, 3, AND 5. 
    !
    !      In other words;
    !      a) MOD(nxl,2)=0 .AND. MOD(nyl,2)=0 .AND nxl>4 .AND. nyl>4 .AND.
    ! 
    !      b)   nxl = 2*(2**A1)*(3**A2)*(5**A3)
    !           nyl = 2*(2**B1)*(3**B2)*(5**B3)
    
    !first guess by adding a margin of 4*lscale
    randxy%nxl = int((xmax-xmin+4.*lscale)/gridsize)+1
    randxy%nyl = int((ymax-ymin+4.*lscale)/gridsize)+1
    
    !Secondly, make sure we fulfil the fft991 requirements
    call adjust_nxl4fft(randxy%nxl)
    call adjust_nxl4fft(randxy%nyl)
    
    ! Allocate and initialise the rest of the fields in randxy.

    ALLOCATE(randxy%pert_xy(np),randxy%x(np),randxy%y(np))
    randxy%pert_xy = 0.
    randxy%nxy     = np
    randxy%x       = xp
    randxy%y       = yp
    ALLOCATE(randxy%pert_field(randxy%nxl,randxy%nyl))
    ALLOCATE(randxy%corr(randxy%nxl,randxy%nyl))
    ALLOCATE(randxy%xfield(randxy%nxl))
    ALLOCATE(randxy%yfield(randxy%nyl))
    randxy%pert_field = 0.
    randxy%corr = 0.
    randxy%xfield(1) = xmin-MODULO(xmin,gridsize)
    DO ix=2,randxy%nxl
        randxy%xfield(ix) = randxy%xfield(1)+gridsize*ix
    ENDDO
    randxy%yfield(1) = ymin-MODULO(ymin,gridsize)
    DO jy=2,randxy%nyl
        randxy%yfield(jy) = randxy%yfield(1)+gridsize*jy
    ENDDO
    randxy%gridsize   = gridsize
    randxy%lscale     = lscale
    randxy%index_corr = index_corr
    randxy%kmax=randxy%nxl/2
    randxy%lmax=randxy%nyl/2
    ALLOCATE(randxy%spec_dens_2D(0:randxy%kmax,-randxy%lmax:randxy%lmax))
    ALLOCATE(randxy%trigsx(3*randxy%nxl/2+1))
    ALLOCATE(randxy%trigsy(3*randxy%nyl/2+1))

    ! Initialize the FFT help arrays 

    call set99(randxy%trigsx,randxy%ifaxx,randxy%nxl)
    call set99(randxy%trigsy,randxy%ifaxy,randxy%nyl)

    ! Generate correlation in physical space and transform
    ! to spectral density in spectral space

    call corr_to_spec(index_corr,lscale,randxy%nxl,randxy%nyl,gridsize, &
                        randxy%kmax,randxy%lmax,randxy%corr,randxy%spec_dens_2D, &
                        randxy%ifaxx,randxy%ifaxy,randxy%trigsx,randxy%trigsy)
                        
    ! Generate an initial sample
    call random_field(randxy%nxl,randxy%nyl,randxy%kmax,randxy%lmax, &
                         randxy%spec_dens_2D,randxy%pert_field,randxy%ifaxx, &
                         randxy%ifaxy,randxy%trigsx,randxy%trigsy)

    ! interpolate to the x,y coordinates
    call interpol_grid2point(randxy%nxy,randxy%x,randxy%y,randxy%pert_xy, &
                           randxy%nxl,randxy%nyl, &
                           randxy%xfield,randxy%yfield,randxy%pert_field)                         
    return                    
  END SUBROUTINE init_randxy_data
  
  !----------------------------------------------------------------------------------------
  !> Subroutine that deallocates any allocated memory in a randxy_data variable
  SUBROUTINE deallocate_randxy_data(randxy)
    TYPE(randxy_data), INTENT(INOUT) :: randxy
    IF(allocated(randxy%pert_xy))DEALLOCATE(randxy%pert_xy)
    IF(allocated(randxy%x))DEALLOCATE(randxy%x)
    IF(allocated(randxy%y))DEALLOCATE(randxy%y)
    IF(allocated(randxy%pert_field))DEALLOCATE(randxy%pert_field)
    IF(allocated(randxy%xfield))DEALLOCATE(randxy%xfield)
    IF(allocated(randxy%yfield))DEALLOCATE(randxy%yfield)
    IF(allocated(randxy%corr))DEALLOCATE(randxy%corr)
    IF(allocated(randxy%spec_dens_2D))DEALLOCATE(randxy%spec_dens_2D)
    IF(allocated(randxy%trigsx))DEALLOCATE(randxy%trigsx)
    IF(allocated(randxy%trigsy))DEALLOCATE(randxy%trigsy)
  END SUBROUTINE deallocate_randxy_data
  
!------------------------------------------------------------------------------------------
!> Adjust the size of random field to fulfill requirement of subroutine fft991
!
! Short description needed!
! Written by NG and translated from F77 to F90 by DG.
!------------------------------------------------------------------------------------------
  SUBROUTINE adjust_nxl4fft(nxl)
    INTEGER, INTENT(INOUT) :: nxl
    INTEGER                :: nxlfact
    LOGICAL                :: nxlfound
    !   2) fft991 require that nxl and nyl MUST BE AN EVEN NUMBER GREATER THAN 4 THAT HAS
    !      NO OTHER FACTORS EXCEPT POSSIBLY POWERS OF 2, 3, AND 5. 
    !
    !      In other words;
    !      a) MOD(nxl,2)=0 .AND. MOD(nyl,2)=0 .AND nxl>4 .AND. nyl>4 .AND.
    ! 
    !      b)   nxl = 2*(2**A1)*(3**A2)*(5**A3)
    !           nyl = 2*(2**B1)*(3**B2)*(5**B3)
    
    !simple solution, increase nxl with 1 until all conditions are fulfilled
    nxlfound = .FALSE.
    DO WHILE(.NOT.nxlfound)
        
        !a) check if it's an even number larger than 4
        IF( (MOD(nxl,2).EQ.0) .AND. (nxl.GT.4))THEN
            
            !b) check if it got any factors, other than powers of 2, 3, and 5
            
            ! b.1) look for factor 2
            nxlfact = nxl
            DO WHILE(MOD(nxlfact,2).EQ.0 .AND. nxlfact.GT.2)
                nxlfact=nxlfact/2
            ENDDO
            
            IF(nxlfact.EQ.2)THEN
                nxlfound = .TRUE.
            ELSE
                ! b.2) look for factor 3
                DO WHILE(MOD(nxlfact,3).EQ.0 .AND. nxlfact.GT.3)
                    nxlfact=nxlfact/3
                ENDDO
                IF(nxlfact.EQ.3)THEN
                    nxlfound = .TRUE.
                ELSE
                    ! b.3) look for factor 5
                    DO WHILE(MOD(nxlfact,5).EQ.0 .AND. nxlfact.GT.5)
                        nxlfact=nxlfact/5
                    ENDDO
                    IF(nxlfact.EQ.5)THEN
                        nxlfound = .TRUE.
                    ELSE
                        nxl = nxl+1
                    ENDIF
                ENDIF
            ENDIF 
        ELSE
            nxl = nxl + 1
        ENDIF
    ENDDO
   
    RETURN
  END SUBROUTINE adjust_nxl4fft

!------------------------------------------------------------------------------------------
!> Generates a sample of a 2D spatially correlated random field with mean 0 and stdev 1
! Written by NG and translated from F77 to F90 by DG.
!------------------------------------------------------------------------------------------
  subroutine random_field(nxl,nyl,kmax,lmax,spec_dens_2D,pert_field, &
                      ifaxx,ifaxy,trigsx,trigsy)
!c
      implicit none
      integer nxl,nyl,kmax,lmax
      real spec_dens_2D(0:kmax,-lmax:lmax)
      real pert_field(nxl,nyl)
      integer ifaxx(13),ifaxy(13)
      real trigsx(3*nxl/2+1),   trigsy(3*nyl/2+1)
!      real rgauss
!      external rgauss
!c
!c-----------------------------------------------------------------------------
!c
      integer k,l
      real zmult
      complex random_field_spec(0:kmax,-lmax:lmax)
!c
!c-----------------------------------------------------------------------------
!c 
      do k=0,kmax
         do l=-lmax,lmax
            zmult = sqrt(spec_dens_2D(k,l)/2.)
            random_field_spec(k,l) = cmplx(rgauss()*zmult,rgauss()*zmult)
         end do
      end do
!c
      call fft2d(.true.,nxl,nyl,pert_field,            &
                      kmax,lmax,random_field_spec,    &
                      ifaxx,ifaxy,trigsx,trigsy)
!c
      return
      end subroutine random_field
!------------------------------------------------------------------------------------------
!> Generate correlation in physical space and transform to spectral density in spectral space.
! Written by NG and translated from F77 to F90 by DG.
!------------------------------------------------------------------------------------------
subroutine corr_to_spec(index_corr,lscale,              &
                             nxl,nyl,gridsize,          &
                             kmax,lmax,                 &
                             corr,spec_dens_2D,         &
                             ifaxx,ifaxy,trigsx,trigsy)
!c
      implicit none
!c
      integer index_corr
      real lscale
      integer nxl,nyl
      real gridsize
      integer kmax,lmax
      real corr(nxl,nyl)
      real spec_dens_2D(0:kmax,-lmax:lmax)
      integer ifaxx(13),ifaxy(13)
      real trigsx(3*nxl/2+1),   trigsy(3*nyl/2+1)
!c
!c------------------------------------------------------------------------------
!c
      integer k,l
      real sum_spec
      complex spec(0:kmax,-lmax:lmax)
      real spec_eps
      parameter (spec_eps = 0.000001) 
!c
!c------------------------------------------------------------------------------
!c
!c Generate correlation function in physical space
!c
      call gen_corr(index_corr,       &
                   gridsize,lscale,   &
                   nxl,nyl,           &
                   corr)
!c
!c 2D Fourier transform to spectal space
!c
      call fft2d(.false.,nxl,nyl,corr,kmax,lmax,spec,  &
                      ifaxx,ifaxy,trigsx,trigsy)
!c
!c Extract 2D spectral density
!c and adjust negative spectral densities caused by 
!c truncation errors
!c
      do k=0,kmax
         do l=-lmax,lmax
            spec_dens_2D(k,l) = max(real(spec(k,l)),spec_eps)
         end do
      end do
!c
!c     Normalize the derived correlation spectrum
!c     in the form needed for random number generation
!c
      sum_spec = 0.
!c
      do k=0,kmax
         do l=-lmax,lmax
            if (k.eq.0) then
               sum_spec = sum_spec +    &
                   spec_dens_2D(k,l) 
            else
               sum_spec = sum_spec +    &
                  2*spec_dens_2D(k,l)
            endif
         end do
      end do
!c
      do k=0,kmax
         do l=-lmax,lmax
            spec_dens_2D(k,l) = spec_dens_2D(k,l)  &
                      /  sum_spec 
         end do
      end do
!c
      return
!c
end subroutine corr_to_spec
!------------------------------------------------------------------------------
!> Generate correlation function in physical space
!------------------------------------------------------------------------------
subroutine gen_corr(index_corr,gridsize,lscale,nxl,nyl,corr)
      implicit none
      integer index_corr
      real gridsize,lscale
      integer nxl,nyl
      real corr(nxl,nyl)
!c
!c-----------------------------------------------------------------------------
!c
      integer i,j
      real cpar,dx,dy,dist,zx,zxl,zz
!c
!c----------------------------------------------------------------------------
!c
      cpar = lscale 
      if (index_corr.eq.2) cpar = lscale*sqrt(10./3.)
!c
      do j=1,nyl
         do i=1,nxl
            dx = min( abs(i-1), abs(nxl+1-i) )
            dy = min( abs(j-1), abs(nyl+1-j) )
!c           dx = min( abs(i), abs(nxl-i) )
!c           dy = min( abs(j), abs(nyl-j) )
            dist = sqrt( dx**2 + dy**2 )*gridsize
!c
!c Gaussian correlation function
!c
            if (index_corr.eq.1) then
!c
               corr(i,j) = exp( - 0.5*(dist/cpar)**2 )
!c
!c Compact 5th order polynomial
!c
            else if (index_corr.eq.2) then
!c
               zx = dist/cpar
               zxl = dist/lscale
!c
               if (dist.le.cpar) then
!c                 
                  zz = -1./4.*zx*zx*zx*zx*zx  &
                        +1./2.*zx*zx*zx*zx    & 
                        +5./8.*zx*zx*zx       &
                        -5./3.*zx*zx          &
                        +1.                 
!c
               elseif ((dist.gt.cpar).and.(dist.le.(2*cpar))) then
!c
                  zz = 1./12.*zx*zx*zx*zx*zx    &
                       -1./2.*zx*zx*zx*zx       &
                       +5./8.*zx*zx*zx          &
                       +5./3.*zx*zx             &
                       -5.*zx                   &
                       +4.                      &
                       -2./3.*(1./zx)      
!c
               else
!c
                  zz = 0.
!c
               endif
!c
               zz = zz/(1+1./2.*zxl*zxl) 
               corr(i,j) = zz
!c 
!c Power law correlation
!c
            else if (index_corr.eq.3) then
!c
               zx = dist/cpar
               zz = 1.+1./2.*zx*zx
               zz = 1./zz
               corr(i,j) = zz   
!c
            else
!c
               write(6,*)' Subroutine gen_corr: correlation function'
               write(6,*)' for index_corr =',index_corr
               write(6,*)' is not defined!'
               stop
!c
            endif  
!c
         end do
      end do
!c
      return 
!c
end subroutine gen_corr


!-----------------------------------------------------------------------------------------
! SPATIAL INTEROLATION BLOCK BEGINNING
!-----------------------------------------------------------------------------------------
!SUBROUTINE interpol_grid2point
!> Interpolation from regular grid Z(X,Y) to points zp(xp,yp) using bilinear interpolation
!-----------------------------------------------------------------------------------------
  SUBROUTINE interpol_grid2point(np,xp,yp,zp,NX,NY,X,Y,Z)
    INTEGER, INTENT(IN) :: np, NX, NY
    REAL,    INTENT(IN) :: xp(np),yp(np),X(NX),Y(NY),Z(NX,NY)
    REAL,    INTENT(OUT):: zp(np)
    INTEGER             :: IX1,IX2,JY1,JY2,ip,DX1,DY1
    
    !Loop over interpolation points
    DO ip=1,np
      !Indentify the 4 grid nodes surrounding the point
      IX2=1
      DO WHILE(X(IX2).LE.xp(ip))
        IX2=IX2+1
      END DO
      IX1=IX2-1
      JY2=1
      DO WHILE(Y(JY2).LE.yp(ip))
        JY2=JY2+1
      END DO
      JY1=JY2-1

      !Calculate the interpolation weights
      dx1=(xp(ip)-X(IX1))/(X(IX2)-X(IX1))
      dy1=(yp(ip)-Y(JY1))/(Y(JY2)-Y(JY1))
        
      ! interpolation using bilinear interpolation
      zp(ip) = (1.0-dx1)*(1.0-dy1)*Z(IX1,JY1) + dx1*(1.0-dy1)*Z(IX2,JY1) + &
                  dx1*dy1*Z(IX2,JY2) + (1.0-dx1)*dy1*Z(IX1,JY2)
        
    ENDDO
  END SUBROUTINE interpol_grid2point
!
! END OF THE SPATIAL INTERPOLATION BLOCK
!-----------------------------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------------------------
!FFT BLOCK BEGINNING
!--------------------------------------------------------------------------
!SUBROUTINE FFT2D
!> 2D Fourier transform to spectal space
!--------------------------------------------------------------------------
   subroutine fft2d(inv,nxl,nyl,a,kmax,lmax,ahat,ifaxx,ifaxy,trigsx,trigsy)
!c
      implicit none
!c
      logical inv
      integer nxl,nyl
      real a(nxl,nyl)
      integer kmax,lmax
      complex ahat(0:kmax,-lmax:lmax)
      integer ifaxx(13),ifaxy(13)
      real trigsx(3*nxl/2+1),   trigsy(3*nyl/2+1)
!c
!c----------------------------------------------------------------
!c
      integer i,j,isign,k,l,m,n,inc,jump
      real pi,zeps,zce,zsei,zs2ei,x,zcxh,zsxh,zsx,g0,g1,g2,g3,y,zcyh,zsyh,zsy
      real w1(nxl+2,nyl+2),w2(nxl+2,nyl+2)
      real wgt
!c
!c----------------------------------------------------------------
!c
      w1 = 0.
      w2 = 0.
!c
      if( inv ) then
!c
         i = 1 
         do k=0,kmax
            w1(i,1) = real( ahat(k,0) )
            w1(i+1,1) = aimag( ahat(k,0) )
            j = 3
            do l=1,lmax
               w1(i,j)     = 0.5 * ( real( ahat(k,l)  ) +      &
                                    real( ahat(k,-l) )   )
               w1(i,j+1)   = 0.5 * ( aimag( ahat(k,l)  ) -     &
                                    aimag( ahat(k,-l) )   )
               w1(i+1,j)   = 0.5 * ( aimag( ahat(k,l)  ) +     &
                                    aimag( ahat(k,-l) )   )
               w1(i+1,j+1) = 0.5 * ( - real( ahat(k,l)  ) +    &
                                      real( ahat(k,-l) )   )
               j = j + 2
            end do
            i = i + 2
         end do
!c
         isign = +1
         m = nxl+2
         n = nyl
         inc = nxl + 2
         jump = 1
         call fft991(w1,w2,trigsy,ifaxy,inc,jump,n,m,isign)
!c
         isign = +1
         m = nyl
         n = nxl
         inc = 1
         jump = nxl + 2
         call fft991(w1,w2,trigsx,ifaxx,inc,jump,n,m,isign)
!c
         do j=1,nyl
            do i=1,nxl
               a(i,j) = w1(i,j)
            end do
         end do
!c
      else
!c
         do j=1,nyl
            do i=1,nxl
               w1(i,j) = a(i,j)
            end do
         end do
!c
         isign = -1
         m = nyl
         n = nxl
         inc = 1
         jump = nxl + 2
         call fft991(w1,w2,trigsx,ifaxx,inc,jump,n,m,isign)
!c
         isign = -1
         m = nxl+2
         n = nyl
         inc = nxl + 2
         jump = 1
         call fft991(w1,w2,trigsy,ifaxy,inc,jump,n,m,isign)
!c
         i = 1 
         do k=0,kmax
            ahat(k,0) = cmplx( w1(i,1) , w1(i+1,1) )
            j = 3
            do l=1,lmax
               ahat(k,l)  = cmplx(  w1(i,j)   - w1(i+1,j+1) ,   &
                                   w1(i,j+1) + w1(i+1,j)    )
               ahat(k,-l) = cmplx(  w1(i,j)   + w1(i+1,j+1) ,   &
                                 - w1(i,j+1) + w1(i+1,j)    )
               j = j + 2
            end do
            i = i + 2
         end do
!c
      end if
!c
      return
!c
  end subroutine fft2d

!!------------------------------------------------------------------------------------------
!! SUBROUTINE fft99, fft991 + further subroutines: package for Fast Fourier Transform.
!!! 
!!! Originally written by Clive Temperton at ECMWF 1978, and included here by Nils Gustafsson,
!!! as part of the routines for generation of spatially correlated gaussian random numbers.
!!! Translation of the F77 code was made by David Gustafsson.
!!!
!!! DG: -seems like a lot of the code is not used by the random number generations (fft99 for instance)
!!!     -maybe we could remove or comment the code that isnt used? 
!!------------------------------------------------------------------------------------------
!  SUBROUTINE FFT99(A,WORK,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)
!!
!! PURPOSE      PERFORMS MULTIPLE FAST FOURIER TRANSFORMS.  THIS PACKAGE
!!              WILL PERFORM A NUMBER OF SIMULTANEOUS REAL/HALF-COMPLEX
!!              PERIODIC FOURIER TRANSFORMS OR CORRESPONDING INVERSE
!!              TRANSFORMS, I.E.  GIVEN A SET OF REAL DATA VECTORS, THE
!!              PACKAGE RETURNS A SET OF 'HALF-COMPLEX' FOURIER
!!              COEFFICIENT VECTORS, OR VICE VERSA.  THE LENGTH OF THE
!!              TRANSFORMS MUST BE AN EVEN NUMBER GREATER THAN 4 THAT HAS
!!              NO OTHER FACTORS EXCEPT POSSIBLY POWERS OF 2, 3, AND 5.
!!              THIS IS AN ALL FORTRAN VERSION OF THE CRAYLIB PACKAGE
!!              THAT IS MOSTLY WRITTEN IN CAL.
!!
!!              THE PACKAGE FFT99F CONTAINS SEVERAL USER-LEVEL ROUTINES:
!!
!!            SUBROUTINE SET99
!!                AN INITIALIZATION ROUTINE THAT MUST BE CALLED ONCE
!!                BEFORE A SEQUENCE OF CALLS TO THE FFT ROUTINES
!!                (PROVIDED THAT N IS NOT CHANGED).
!!
!!            SUBROUTINES FFT99 AND FFT991
!!                TWO FFT ROUTINES THAT RETURN SLIGHTLY DIFFERENT
!!                ARRANGEMENTS OF THE DATA IN GRIDPOINT SPACE.
!!
!!
!! ACCESS       THIS FORTRAN VERSION MAY BE ACCESSED WITH
!!
!!                   *FORTRAN,P=XLIB,SN=FFT99F
!!
!!              TO ACCESS THE CRAY OBJECT CODE, CALLING THE USER ENTRY
!!              POINTS FROM A CRAY PROGRAM IS SUFFICIENT.  THE SOURCE
!!              FORTRAN AND CAL CODE FOR THE CRAYLIB VERSION MAY BE
!!              ACCESSED USING
!!
!!                   FETCH P=CRAYLIB,SN=FFT99
!!                   FETCH P=CRAYLIB,SN=CAL99
!!
!! USAGE        LET N BE OF THE FORM 2**P * 3**Q * 5**R, WHERE P .GE. 1,
!!              Q .GE. 0, AND R .GE. 0.  THEN A TYPICAL SEQUENCE OF
!!              CALLS TO TRANSFORM A GIVEN SET OF REAL VECTORS OF LENGTH
!!              N TO A SET OF 'HALF-COMPLEX' FOURIER COEFFICIENT VECTORS
!!              OF LENGTH N IS
!!
!!                   DIMENSION IFAX(13),TRIGS(3*N/2+1),A(M*(N+2)),
!!                  +          WORK(M*(N+1))
!!
!!                   CALL SET99 (TRIGS, IFAX, N)
!!                   CALL FFT99 (A,WORK,TRIGS,IFAX,INC,JUMP,N,M,ISIGN)
!!
!!              SEE THE INDIVIDUAL WRITE-UPS FOR SET99, FFT99, AND
!!              FFT991 BELOW, FOR A DETAILED DESCRIPTION OF THE
!!              ARGUMENTS.
!!
!! HISTORY      THE PACKAGE WAS WRITTEN BY CLIVE TEMPERTON AT ECMWF IN
!!              NOVEMBER, 1978.  IT WAS MODIFIED, DOCUMENTED, AND TESTED
!!              FOR NCAR BY RUSS REW IN SEPTEMBER, 1980.
!!
!!-----------------------------------------------------------------------
!!
!! SUBROUTINE SET99 (TRIGS, IFAX, N)
!!
!! PURPOSE      A SET-UP ROUTINE FOR FFT99 AND FFT991.  IT NEED ONLY BE
!!              CALLED ONCE BEFORE A SEQUENCE OF CALLS TO THE FFT
!!              ROUTINES (PROVIDED THAT N IS NOT CHANGED).
!!
!! ARGUMENT     IFAX(13),TRIGS(3*N/2+1)
!! DIMENSIONS
!!
!! ARGUMENTS
!!
!! ON INPUT     TRIGS
!!               A FLOATING POINT ARRAY OF DIMENSION 3*N/2 IF N/2 IS
!!               EVEN, OR 3*N/2+1 IF N/2 IS ODD.
!!
!!              IFAX
!!               AN INTEGER ARRAY.  THE NUMBER OF ELEMENTS ACTUALLY USED
!!               WILL DEPEND ON THE FACTORIZATION OF N.  DIMENSIONING
!!               IFAX FOR 13 SUFFICES FOR ALL N LESS THAN A MILLION.
!!
!!              N
!!               AN EVEN NUMBER GREATER THAN 4 THAT HAS NO PRIME FACTOR
!!               GREATER THAN 5.  N IS THE LENGTH OF THE TRANSFORMS (SEE
!!               THE DOCUMENTATION FOR FFT99 AND FFT991 FOR THE
!!               DEFINITIONS OF THE TRANSFORMS).
!!
!! ON OUTPUT    IFAX
!!               CONTAINS THE FACTORIZATION OF N/2.  IFAX(1) IS THE
!!               NUMBER OF FACTORS, AND THE FACTORS THEMSELVES ARE STORED
!!               IN IFAX(2),IFAX(3),...  IF SET99 IS CALLED WITH N ODD,
!!               OR IF N HAS ANY PRIME FACTORS GREATER THAN 5, IFAX(1)
!!               IS SET TO -99.
!!
!!              TRIGS
!!               AN ARRAY OF TRIGONOMETRIC FUNCTION VALUES SUBSEQUENTLY
!!               USED BY THE FFT ROUTINES.
!!
!!-----------------------------------------------------------------------
!!
!! SUBROUTINE FFT991 (A,WORK,TRIGS,IFAX,INC,JUMP,N,M,ISIGN)
!!                       AND
!! SUBROUTINE FFT99 (A,WORK,TRIGS,IFAX,INC,JUMP,N,M,ISIGN)
!!
!! PURPOSE      PERFORM A NUMBER OF SIMULTANEOUS REAL/HALF-COMPLEX
!!              PERIODIC FOURIER TRANSFORMS OR CORRESPONDING INVERSE
!!              TRANSFORMS, USING ORDINARY SPATIAL ORDER OF GRIDPOINT
!!              VALUES (FFT991) OR EXPLICIT CYCLIC CONTINUITY IN THE
!!              GRIDPOINT VALUES (FFT99).  GIVEN A SET
!!              OF REAL DATA VECTORS, THE PACKAGE RETURNS A SET OF
!!              'HALF-COMPLEX' FOURIER COEFFICIENT VECTORS, OR VICE
!!              VERSA.  THE LENGTH OF THE TRANSFORMS MUST BE AN EVEN
!!              NUMBER THAT HAS NO OTHER FACTORS EXCEPT POSSIBLY POWERS
!!              OF 2, 3, AND 5.  THESE VERSION OF FFT991 AND FFT99 ARE
!!              OPTIMIZED FOR USE ON THE CRAY-1.
!!
!! ARGUMENT     A(M*(N+2)), WORK(M*(N+1)), TRIGS(3*N/2+1), IFAX(13)
!! DIMENSIONS
!!
!! ARGUMENTS
!!
!! ON INPUT     A
!!               AN ARRAY OF LENGTH M*(N+2) CONTAINING THE INPUT DATA
!!               OR COEFFICIENT VECTORS.  THIS ARRAY IS OVERWRITTEN BY
!!               THE RESULTS.
!!
!!              WORK
!!               A WORK ARRAY OF DIMENSION M*(N+1)
!!
!!              TRIGS
!!               AN ARRAY SET UP BY SET99, WHICH MUST BE CALLED FIRST.
!!
!!              IFAX
!!               AN ARRAY SET UP BY SET99, WHICH MUST BE CALLED FIRST.
!!
!!              INC
!!               THE INCREMENT (IN WORDS) BETWEEN SUCCESSIVE ELEMENTS OF
!!               EACH DATA OR COEFFICIENT VECTOR (E.G.  INC=1 FOR
!!               CONSECUTIVELY STORED DATA).
!!
!!              JUMP
!!               THE INCREMENT (IN WORDS) BETWEEN THE FIRST ELEMENTS OF
!!               SUCCESSIVE DATA OR COEFFICIENT VECTORS.  ON THE CRAY-1,
!!               TRY TO ARRANGE DATA SO THAT JUMP IS NOT A MULTIPLE OF 8
!!               (TO AVOID MEMORY BANK CONFLICTS).  FOR CLARIFICATION OF
!!               INC AND JUMP, SEE THE EXAMPLES BELOW.
!!
!!              N
!!               THE LENGTH OF EACH TRANSFORM (SEE DEFINITION OF
!!               TRANSFORMS, BELOW).
!!
!!              M
!!               THE NUMBER OF TRANSFORMS TO BE DONE SIMULTANEOUSLY.
!!
!!              ISIGN
!!               = +1 FOR A TRANSFORM FROM FOURIER COEFFICIENTS TO
!!                    GRIDPOINT VALUES.
!!               = -1 FOR A TRANSFORM FROM GRIDPOINT VALUES TO FOURIER
!!                    COEFFICIENTS.
!!
!! ON OUTPUT    A
!!               IF ISIGN = +1, AND M COEFFICIENT VECTORS ARE SUPPLIED
!!               EACH CONTAINING THE SEQUENCE:
!!
!!               A(0),B(0),A(1),B(1),...,A(N/2),B(N/2)  (N+2 VALUES)
!!
!!               THEN THE RESULT CONSISTS OF M DATA VECTORS EACH
!!               CONTAINING THE CORRESPONDING N+2 GRIDPOINT VALUES:
!!
!!               FOR FFT991, X(0), X(1), X(2),...,X(N-1),0,0.
!!               FOR FFT99, X(N-1),X(0),X(1),X(2),...,X(N-1),X(0).
!!                   (EXPLICIT CYCLIC CONTINUITY)
!!
!!               WHEN ISIGN = +1, THE TRANSFORM IS DEFINED BY:
!!                 X(J)=SUM(K=0,...,N-1)(C(K)*EXP(2*I*J*K*PI/N))
!!                 WHERE C(K)=A(K)+I*B(K) AND C(N-K)=A(K)-I*B(K)
!!                 AND I=SQRT (-1)
!!
!!               IF ISIGN = -1, AND M DATA VECTORS ARE SUPPLIED EACH
!!               CONTAINING A SEQUENCE OF GRIDPOINT VALUES X(J) AS
!!               DEFINED ABOVE, THEN THE RESULT CONSISTS OF M VECTORS
!!               EACH CONTAINING THE CORRESPONDING FOURIER COFFICIENTS
!!               A(K), B(K), 0 .LE. K .LE N/2.
!!
!!               WHEN ISIGN = -1, THE INVERSE TRANSFORM IS DEFINED BY:
!!                 C(K)=(1/N)*SUM(J=0,...,N-1)(X(J)*EXP(-2*I*J*K*PI/N))
!!                 WHERE C(K)=A(K)+I*B(K) AND I=SQRT(-1)
!!
!!               A CALL WITH ISIGN=+1 FOLLOWED BY A CALL WITH ISIGN=-1
!!               (OR VICE VERSA) RETURNS THE ORIGINAL DATA.
!!
!!               NOTE: THE FACT THAT THE GRIDPOINT VALUES X(J) ARE REAL
!!               IMPLIES THAT B(0)=B(N/2)=0.  FOR A CALL WITH ISIGN=+1,
!!               IT IS NOT ACTUALLY NECESSARY TO SUPPLY THESE ZEROS.
!!
!! EXAMPLES      GIVEN 19 DATA VECTORS EACH OF LENGTH 64 (+2 FOR EXPLICIT
!!               CYCLIC CONTINUITY), COMPUTE THE CORRESPONDING VECTORS OF
!!               FOURIER COEFFICIENTS.  THE DATA MAY, FOR EXAMPLE, BE
!!               ARRANGED LIKE THIS:
!!
!! FIRST DATA   A(1)=    . . .                A(66)=             A(70)
!! VECTOR       X(63) X(0) X(1) X(2) ... X(63) X(0)  (4 EMPTY LOCATIONS)
!!
!! SECOND DATA  A(71)=   . . .                                  A(140)
!! VECTOR       X(63) X(0) X(1) X(2) ... X(63) X(0)  (4 EMPTY LOCATIONS)
!!
!!               AND SO ON.  HERE INC=1, JUMP=70, N=64, M=19, ISIGN=-1,
!!               AND FFT99 SHOULD BE USED (BECAUSE OF THE EXPLICIT CYCLIC
!!               CONTINUITY).
!!
!!               ALTERNATIVELY THE DATA MAY BE ARRANGED LIKE THIS:
!!
!!                FIRST         SECOND                          LAST
!!                DATA          DATA                            DATA
!!                VECTOR        VECTOR                          VECTOR
!!
!!                 A(1)=         A(2)=                           A(19)=
!!
!!                 X(63)         X(63)       . . .               X(63)
!!        A(20)=   X(0)          X(0)        . . .               X(0)
!!        A(39)=   X(1)          X(1)        . . .               X(1)
!!                  .             .                               .
!!                  .             .                               .
!!                  .             .                               .
!!
!!               IN WHICH CASE WE HAVE INC=19, JUMP=1, AND THE REMAINING
!!               PARAMETERS ARE THE SAME AS BEFORE.  IN EITHER CASE, EACH
!!               COEFFICIENT VECTOR OVERWRITES THE CORRESPONDING INPUT
!!               DATA VECTOR.
!!
!!-----------------------------------------------------------------------
!!
!! $Id: fft99.F90,v 1.4 2007/04/26 14:38:12 wputman Exp $
!! $Author: wputman $
!!
!      !DG use shr_kind_mod, only : r8 => shr_kind_r8
!      implicit none
!
!      INTEGER  :: IFAX(13),INC,JUMP,N,LOT,ISIGN
!      !REAL(R8) :: A(LOT*(N+2)),WORK(LOT*(N+1)), TRIGS(3*N/2+1)
!      REAL :: A(LOT*(N+2)),WORK(LOT*(N+1)), TRIGS(3*N/2+1)
!
!!
!!     SUBROUTINE "FFT99" - MULTIPLE FAST REAL PERIODIC TRANSFORM
!!     CORRESPONDING TO OLD SCALAR ROUTINE FFT9
!!     PROCEDURE USED TO CONVERT TO HALF-LENGTH COMPLEX TRANSFORM
!!     IS GIVEN BY COOLEY, LEWIS AND WELCH (J. SOUND VIB., VOL. 12
!!     (1970), 315-337)
!!
!!     A IS THE ARRAY CONTAINING INPUT AND OUTPUT DATA
!!     WORK IS AN AREA OF SIZE (N+1)*LOT
!!     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG FUNCTION VALUES
!!     IFAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N/2
!!     INC IS THE INCREMENT WITHIN EACH DATA 'VECTOR'
!!         (E.G. INC=1 FOR CONSECUTIVELY STORED DATA)
!!     JUMP IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR
!!     N IS THE LENGTH OF THE DATA VECTORS
!!     LOT IS THE NUMBER OF DATA VECTORS
!!     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT
!!           = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL
!!
!!     ORDERING OF COEFFICIENTS:
!!         A(0),B(0),A(1),B(1),A(2),B(2),...,A(N/2),B(N/2)
!!         WHERE B(0)=B(N/2)=0; (N+2) LOCATIONS REQUIRED
!!
!!     ORDERING OF DATA:
!!         X(N-1),X(0),X(1),X(2),...,X(N),X(0)
!!         I.E. EXPLICIT CYCLIC CONTINUITY; (N+2) LOCATIONS REQUIRED
!!
!!     VECTORIZATION IS ACHIEVED ON CRAY BY DOING THE TRANSFORMS IN
!!     PARALLEL
!!
!!     *** N.B. N IS ASSUMED TO BE AN EVEN NUMBER
!!
!!     DEFINITION OF TRANSFORMS:
!!     -------------------------
!!
!!     ISIGN=+1: X(J)=SUM(K=0,...,N-1)(C(K)*EXP(2*I*J*K*PI/N))
!!         WHERE C(K)=A(K)+I*B(K) AND C(N-K)=A(K)-I*B(K)
!!
!!     ISIGN=-1: A(K)=(1/N)*SUM(J=0,...,N-1)(X(J)*COS(2*J*K*PI/N))
!!               B(K)=-(1/N)*SUM(J=0,...,N-1)(X(J)*SIN(2*J*K*PI/N))
!!
!!
!!
!!
!      INTEGER  :: NFAX, NX, NH, INK, IGO, IBASE, JBASE, I, J, K, L, M, &
!                  IA, JA, LA, IB
!
!      NFAX=IFAX(1)
!      NX=N+1
!      NH=N/2
!      INK=INC+INC
!      IF (ISIGN.EQ.+1) GO TO 30
!!
!!     IF NECESSARY, TRANSFER DATA TO WORK AREA
!      IGO=50
!      IF (MOD(NFAX,2).EQ.1) GOTO 40
!      IBASE=INC+1
!      JBASE=1
!      DO 20 L=1,LOT
!      I=IBASE
!      J=JBASE
!
!      DO 10 M=1,N
!      WORK(J)=A(I)
!      I=I+INC
!      J=J+1
!   10 CONTINUE
!      IBASE=IBASE+JUMP
!      JBASE=JBASE+NX
!   20 CONTINUE
!!
!      IGO=60
!      GO TO 40
!!
!!     PREPROCESSING (ISIGN=+1)
!!     ------------------------
!!
!   30 CONTINUE
!      CALL FFT99A(A,WORK,TRIGS,INC,JUMP,N,LOT)
!      IGO=60
!!
!!     COMPLEX TRANSFORM
!!     -----------------
!!
!   40 CONTINUE
!      IA=INC+1
!      LA=1
!      DO 80 K=1,NFAX
!      IF (IGO.EQ.60) GO TO 60
!   50 CONTINUE
!      CALL VPASSM(A(IA),A(IA+INC),WORK(1),WORK(2),TRIGS, &
!        INK,2,JUMP,NX,LOT,NH,IFAX(K+1),LA)
!      IGO=60
!      GO TO 70
!   60 CONTINUE
!      CALL VPASSM(WORK(1),WORK(2),A(IA),A(IA+INC),TRIGS, &
!         2,INK,NX,JUMP,LOT,NH,IFAX(K+1),LA)
!      IGO=50
!   70 CONTINUE
!      LA=LA*IFAX(K+1)
!   80 CONTINUE
!!
!      IF (ISIGN.EQ.-1) GO TO 130
!!
!!     IF NECESSARY, TRANSFER DATA FROM WORK AREA
!      IF (MOD(NFAX,2).EQ.1) GO TO 110
!      IBASE=1
!      JBASE=IA
!      DO 100 L=1,LOT
!      I=IBASE
!      J=JBASE
!
!      DO 90 M=1,N
!      A(J)=WORK(I)
!      I=I+1
!      J=J+INC
!   90 CONTINUE
!      IBASE=IBASE+NX
!      JBASE=JBASE+JUMP
!  100 CONTINUE
!!
!!     FILL IN CYCLIC BOUNDARY POINTS
!  110 CONTINUE
!      IA=1
!      IB=N*INC+1
!
!      DO 120 L=1,LOT
!      A(IA)=A(IB)
!      A(IB+INC)=A(IA+INC)
!      IA=IA+JUMP
!      IB=IB+JUMP
!  120 CONTINUE
!      GO TO 140
!!
!!     POSTPROCESSING (ISIGN=-1):
!!     --------------------------
!!
!  130 CONTINUE
!      CALL FFT99B(WORK,A,TRIGS,INC,JUMP,N,LOT)
!!
!  140 CONTINUE
!      RETURN
!      END SUBROUTINE
!!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>Preprocessing for fft99 routine (spectral to grid point transform)
  SUBROUTINE FFT99A(A,WORK,TRIGS,INC,JUMP,N,LOT)
      !DG use shr_kind_mod, only : r8 => shr_kind_r8
      implicit none

      !REAL(R8) :: A(*),WORK(*),TRIGS(*)
      REAL :: A(*),WORK(*),TRIGS(*)
      INTEGER  :: INC,JUMP,N,LOT

!
!     SUBROUTINE FFT99A - PREPROCESSING STEP FOR FFT99, ISIGN=+1
!     (SPECTRAL TO GRIDPOINT TRANSFORM)
!
!DG      REAL(R8) :: C, S
      REAL :: C, S
      INTEGER  :: NH, NX, INK, IA, IB, JA, JB, IABASE, JABASE, K, L, &
                  IBBASE, JBBASE

      NH=N/2
      NX=N+1
      INK=INC+INC
!
!     A(0) AND A(N/2)
      IA=1
      IB=N*INC+1
      JA=1
      JB=2

      DO 10 L=1,LOT
      WORK(JA)=A(IA)+A(IB)
      WORK(JB)=A(IA)-A(IB)
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
   10 CONTINUE
!
!     REMAINING WAVENUMBERS
      IABASE=2*INC+1
      IBBASE=(N-2)*INC+1
      JABASE=3
      JBBASE=N-1
!
      DO 30 K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)

      DO 20 L=1,LOT
      WORK(JA)=(A(IA)+A(IB))-(S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JB)=(A(IA)+A(IB))+(S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JA+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))+(A(IA+INC)-A(IB+INC))
      WORK(JB+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))-(A(IA+INC)-A(IB+INC))
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
   20 CONTINUE
      IABASE=IABASE+INK
      IBBASE=IBBASE-INK
      JABASE=JABASE+2
      JBBASE=JBBASE-2
   30 CONTINUE
!
      IF (IABASE.NE.IBBASE) GO TO 50
!     WAVENUMBER N/4 (IF IT EXISTS)
      IA=IABASE
      JA=JABASE

      DO 40 L=1,LOT
      WORK(JA)=2.0*A(IA)
      WORK(JA+1)=-2.0*A(IA+INC)
      IA=IA+JUMP
      JA=JA+NX
   40 CONTINUE
!
   50 CONTINUE
      RETURN
  END SUBROUTINE
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>Postprocessing for fft99 routine (grid point to spectral transform)
  SUBROUTINE FFT99B(WORK,A,TRIGS,INC,JUMP,N,LOT)
      !DG use shr_kind_mod, only : r8 => shr_kind_r8
      implicit none

      !DGREAL(R8) :: WORK(*),A(*),TRIGS(*)
      REAL :: WORK(*),A(*),TRIGS(*)
      INTEGER  :: INC,JUMP,N,LOT

!
!     SUBROUTINE FFT99B - POSTPROCESSING STEP FOR FFT99, ISIGN=-1
!     (GRIDPOINT TO SPECTRAL TRANSFORM)
!
!DG      REAL(R8) :: SCALE, C, S
      REAL :: SCALE, C, S
      INTEGER  :: NH, NX, INK, IA, IB, JA, JB, K, L, &
                  IABASE, JABASE, IBBASE, JBBASE

      NH=N/2
      NX=N+1
      INK=INC+INC
!
!     A(0) AND A(N/2)
      SCALE=1.0/FLOAT(N)
      IA=1
      IB=2
      JA=1
      JB=N*INC+1

      DO 10 L=1,LOT
      A(JA)=SCALE*(WORK(IA)+WORK(IB))
      A(JB)=SCALE*(WORK(IA)-WORK(IB))
      A(JA+INC)=0.0
      A(JB+INC)=0.0
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
   10 CONTINUE
!
!     REMAINING WAVENUMBERS
      SCALE=0.5*SCALE
      IABASE=3
      IBBASE=N-1
      JABASE=2*INC+1
      JBBASE=(N-2)*INC+1
!
      DO 30 K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)

      DO 20 L=1,LOT
      A(JA)=SCALE*((WORK(IA)+WORK(IB)) &
        +(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JB)=SCALE*((WORK(IA)+WORK(IB)) &
        -(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JA+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1))) &
         +(WORK(IB+1)-WORK(IA+1)))
      A(JB+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1))) &
         -(WORK(IB+1)-WORK(IA+1)))
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
   20 CONTINUE
      IABASE=IABASE+2
      IBBASE=IBBASE-2
      JABASE=JABASE+INK
      JBBASE=JBBASE-INK
   30 CONTINUE
!
      IF (IABASE.NE.IBBASE) GO TO 50
!     WAVENUMBER N/4 (IF IT EXISTS)
      IA=IABASE
      JA=JABASE
      SCALE=2.0*SCALE

      DO 40 L=1,LOT
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=-SCALE*WORK(IA+1)
      IA=IA+NX
      JA=JA+JUMP
   40 CONTINUE
!
   50 CONTINUE
      RETURN
  END SUBROUTINE
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>Multiple real/half-complex periodic Fast Fourier Transform
  SUBROUTINE FFT991(A,WORK,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)
      !DG use shr_kind_mod, only : r8 => shr_kind_r8
      implicit none

      !DG REAL(R8) :: A(*),WORK(*),TRIGS(*)
      REAL :: A(*),WORK(*),TRIGS(*)
      INTEGER  :: IFAX(13), INC, JUMP, N, LOT, ISIGN

!
!     SUBROUTINE "FFT991" - MULTIPLE REAL/HALF-COMPLEX PERIODIC
!     FAST FOURIER TRANSFORM
!
!     SAME AS FFT99 EXCEPT THAT ORDERING OF DATA CORRESPONDS TO
!     THAT IN MRFFT2
!
!     PROCEDURE USED TO CONVERT TO HALF-LENGTH COMPLEX TRANSFORM
!     IS GIVEN BY COOLEY, LEWIS AND WELCH (J. SOUND VIB., VOL. 12
!     (1970), 315-337)
!
!     A IS THE ARRAY CONTAINING INPUT AND OUTPUT DATA
!     WORK IS AN AREA OF SIZE (N+1)*LOT
!     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG FUNCTION VALUES
!     IFAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N/2
!     INC IS THE INCREMENT WITHIN EACH DATA 'VECTOR'
!         (E.G. INC=1 FOR CONSECUTIVELY STORED DATA)
!     JUMP IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR
!     N IS THE LENGTH OF THE DATA VECTORS
!     LOT IS THE NUMBER OF DATA VECTORS
!     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT
!           = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL
!
!     ORDERING OF COEFFICIENTS:
!         A(0),B(0),A(1),B(1),A(2),B(2),...,A(N/2),B(N/2)
!         WHERE B(0)=B(N/2)=0; (N+2) LOCATIONS REQUIRED
!
!     ORDERING OF DATA:
!         X(0),X(1),X(2),...,X(N-1)
!
!     VECTORIZATION IS ACHIEVED ON CRAY BY DOING THE TRANSFORMS IN
!     PARALLEL
!
!     *** N.B. N IS ASSUMED TO BE AN EVEN NUMBER
!
!     DEFINITION OF TRANSFORMS:
!     -------------------------
!
!     ISIGN=+1: X(J)=SUM(K=0,...,N-1)(C(K)*EXP(2*I*J*K*PI/N))
!         WHERE C(K)=A(K)+I*B(K) AND C(N-K)=A(K)-I*B(K)
!
!     ISIGN=-1: A(K)=(1/N)*SUM(J=0,...,N-1)(X(J)*COS(2*J*K*PI/N))
!               B(K)=-(1/N)*SUM(J=0,...,N-1)(X(J)*SIN(2*J*K*PI/N))
!
      INTEGER  :: NFAX, NX, NH, INK, IGO, IBASE, JBASE, I, J, K, L, M, &
                  IA, LA, IB

      NFAX=IFAX(1)
      NX=N+1
      NH=N/2
      INK=INC+INC
      IF (ISIGN.EQ.+1) GO TO 30

!     IF NECESSARY, TRANSFER DATA TO WORK AREA
      IGO=50
      IF (MOD(NFAX,2).EQ.1) GOTO 40
      IBASE=1
      JBASE=1
      DO 20 L=1,LOT
      I=IBASE
      J=JBASE

      DO 10 M=1,N
      WORK(J)=A(I)
      I=I+INC
      J=J+1
   10 CONTINUE
      IBASE=IBASE+JUMP
      JBASE=JBASE+NX
   20 CONTINUE

      IGO=60
      GO TO 40

!     PREPROCESSING (ISIGN=+1)
!     ------------------------

   30 CONTINUE
      CALL FFT99A(A,WORK,TRIGS,INC,JUMP,N,LOT)
      IGO=60

!     COMPLEX TRANSFORM
!     -----------------

   40 CONTINUE
      IA=1
      LA=1
      DO 80 K=1,NFAX
      IF (IGO.EQ.60) GO TO 60
   50 CONTINUE
      CALL VPASSM(A(IA),A(IA+INC),WORK(1),WORK(2),TRIGS, &
        INK,2,JUMP,NX,LOT,NH,IFAX(K+1),LA)
      IGO=60
      GO TO 70
   60 CONTINUE
      CALL VPASSM(WORK(1),WORK(2),A(IA),A(IA+INC),TRIGS, &
         2,INK,NX,JUMP,LOT,NH,IFAX(K+1),LA)
      IGO=50
   70 CONTINUE
      LA=LA*IFAX(K+1)
   80 CONTINUE

      IF (ISIGN.EQ.-1) GO TO 130

!     IF NECESSARY, TRANSFER DATA FROM WORK AREA
      IF (MOD(NFAX,2).EQ.1) GO TO 110
      IBASE=1
      JBASE=1
      DO 100 L=1,LOT
      I=IBASE
      J=JBASE

      DO 90 M=1,N
      A(J)=WORK(I)
      I=I+1
      J=J+INC
   90 CONTINUE
      IBASE=IBASE+NX
      JBASE=JBASE+JUMP
  100 CONTINUE

!     FILL IN ZEROS AT END
  110 CONTINUE
      IB=N*INC+1

      DO 120 L=1,LOT
      A(IB)=0.0
      A(IB+INC)=0.0
      IB=IB+JUMP
  120 CONTINUE
      GO TO 140

!     POSTPROCESSING (ISIGN=-1):
!     --------------------------

  130 CONTINUE
      CALL FFT99B(WORK,A,TRIGS,INC,JUMP,N,LOT)

  140 CONTINUE
      RETURN
  END SUBROUTINE
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>An initialisation routine that must be called once before a sequence of calls
!!to the FFT routines (provided that N is not changed).
  SUBROUTINE SET99 (TRIGS, IFAX, N)
      !DG use shr_kind_mod, only : r8 => shr_kind_r8
      implicit none

      !DG REAL(R8) :: TRIGS(*)
      REAL :: TRIGS(*)
      INTEGER  :: IFAX(13), N

!
! MODE 3 IS USED FOR REAL/HALF-COMPLEX TRANSFORMS.  IT IS POSSIBLE
! TO DO COMPLEX/COMPLEX TRANSFORMS WITH OTHER VALUES OF MODE, BUT
! DOCUMENTATION OF THE DETAILS WERE NOT AVAILABLE WHEN THIS ROUTINE
! WAS WRITTEN.
!
      INTEGER  :: MODE, I

      DATA MODE /3/
      CALL FAX(IFAX, N, MODE)
      I = IFAX(1)
      IF (IFAX(I+1) .GT. 5 .OR. N .LE. 4) IFAX(1) = -99
      IF (IFAX(1) .LE. 0 ) THEN 
        WRITE(6,*) ' SET99 -- INVALID N'
        STOP'SET99'
      ENDIF
      CALL FFTRIG(TRIGS, N, MODE)
      RETURN
  END SUBROUTINE
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
  SUBROUTINE FAX(IFAX,N,MODE)
      !DG use shr_kind_mod, only : r8 => shr_kind_r8
      implicit none

      INTEGER  :: IFAX(10), N, MODE 

      INTEGER  :: NN, I, K, L, II, ISTOP, ITEM, INC, NFAX

      NN=N
      IF (IABS(MODE).EQ.1) GO TO 10
      IF (IABS(MODE).EQ.8) GO TO 10
      NN=N/2
      IF ((NN+NN).EQ.N) GO TO 10
      IFAX(1)=-99
      RETURN
   10 K=1
!     TEST FOR FACTORS OF 4
   20 IF (MOD(NN,4).NE.0) GO TO 30
      K=K+1
      IFAX(K)=4
      NN=NN/4
      IF (NN.EQ.1) GO TO 80
      GO TO 20
!     TEST FOR EXTRA FACTOR OF 2
   30 IF (MOD(NN,2).NE.0) GO TO 40
      K=K+1
      IFAX(K)=2
      NN=NN/2
      IF (NN.EQ.1) GO TO 80
!     TEST FOR FACTORS OF 3
   40 IF (MOD(NN,3).NE.0) GO TO 50
      K=K+1
      IFAX(K)=3
      NN=NN/3
      IF (NN.EQ.1) GO TO 80
      GO TO 40
!     NOW FIND REMAINING FACTORS
   50 L=5
      INC=2
!     INC ALTERNATELY TAKES ON VALUES 2 AND 4
   60 IF (MOD(NN,L).NE.0) GO TO 70
      K=K+1
      IFAX(K)=L
      NN=NN/L
      IF (NN.EQ.1) GO TO 80
      GO TO 60
   70 L=L+INC
      INC=6-INC
      GO TO 60
   80 IFAX(1)=K-1
!     IFAX(1) CONTAINS NUMBER OF FACTORS
      NFAX=IFAX(1)
!     SORT FACTORS INTO ASCENDING ORDER
      IF (NFAX.EQ.1) GO TO 110
      DO 100 II=2,NFAX
      ISTOP=NFAX+2-II
      DO 90 I=2,ISTOP
      IF (IFAX(I+1).GE.IFAX(I)) GO TO 90
      ITEM=IFAX(I)
      IFAX(I)=IFAX(I+1)
      IFAX(I+1)=ITEM
   90 CONTINUE
  100 CONTINUE
  110 CONTINUE
      RETURN
  END SUBROUTINE
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

  SUBROUTINE FFTRIG(TRIGS,N,MODE)
      !DG use shr_kind_mod, only : r8 => shr_kind_r8
      implicit none
      !DG REAL(R8) :: TRIGS(*)
      REAL :: TRIGS(*)
      INTEGER  :: N, MODE

!      REAL(R8) :: PI, DEL, ANGLE
      REAL :: PI, DEL, ANGLE
      INTEGER  :: IMODE, NN, I, L, NH, LA

!BMP  PI=2.0*ASIN(1.0)
!DG      PI=3.14159265358979323846_r8
      PI=3.14159265358979323846
      IMODE=IABS(MODE)
      NN=N
      IF (IMODE.GT.1.AND.IMODE.LT.6) NN=N/2
      DEL=(PI+PI)/FLOAT(NN)
      L=NN+NN
      DO 10 I=1,L,2
      ANGLE=0.5*FLOAT(I-1)*DEL
      TRIGS(I)=COS(ANGLE)
      TRIGS(I+1)=SIN(ANGLE)
   10 CONTINUE
      IF (IMODE.EQ.1) RETURN
      IF (IMODE.EQ.8) RETURN
      DEL=0.5*DEL
      NH=(NN+1)/2
      L=NH+NH
      LA=NN+NN
      DO 20 I=1,L,2
      ANGLE=0.5*FLOAT(I-1)*DEL
      TRIGS(LA+I)=COS(ANGLE)
      TRIGS(LA+I+1)=SIN(ANGLE)
   20 CONTINUE
      IF (IMODE.LE.3) RETURN
      DEL=0.5*DEL
      LA=LA+NN
      IF (MODE.EQ.5) GO TO 40
      DO 30 I=2,NN
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=2.0*SIN(ANGLE)
   30 CONTINUE
      RETURN
   40 CONTINUE
      DEL=0.5*DEL
      DO 50 I=2,N
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=SIN(ANGLE)
   50 CONTINUE
      RETURN
  END SUBROUTINE
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!>Performs one pass through data as part of the multiple complex FFT routine,
   SUBROUTINE VPASSM(A,B,C,D,TRIGS,INC1,INC2,INC3,INC4,LOT,N,IFAC,LA)
      !DG use shr_kind_mod, only : r8 => shr_kind_r8
      implicit none
      !DG REAL(R8) :: A(*),B(*),C(*),D(*),TRIGS(*)
      REAL :: A(*),B(*),C(*),D(*),TRIGS(*)
      INTEGER  :: INC1,INC2,INC3,INC4,LOT,N,IFAC,LA

!     SUBROUTINE "VPASSM" - MULTIPLE VERSION OF "VPASSA"
!     PERFORMS ONE PASS THROUGH DATA
!     AS PART OF MULTIPLE COMPLEX FFT ROUTINE
!     A IS FIRST REAL INPUT VECTOR
!     B IS FIRST IMAGINARY INPUT VECTOR
!     C IS FIRST REAL OUTPUT VECTOR
!     D IS FIRST IMAGINARY OUTPUT VECTOR
!     TRIGS IS PRECALCULATED TABLE OF SINES " COSINES
!     INC1 IS ADDRESSING INCREMENT FOR A AND B
!     INC2 IS ADDRESSING INCREMENT FOR C AND D
!     INC3 IS ADDRESSING INCREMENT BETWEEN A"S & B"S
!     INC4 IS ADDRESSING INCREMENT BETWEEN C"S & D"S
!     LOT IS THE NUMBER OF VECTORS
!     N IS LENGTH OF VECTORS
!     IFAC IS CURRENT FACTOR OF N
!     LA IS PRODUCT OF PREVIOUS FACTORS


!DG      REAL(R8) :: SIN36, COS36, SIN72, COS72, SIN60, &
!DG                  C1, S1, C2, S2, C3, S3, C4, S4
      REAL :: SIN36, COS36, SIN72, COS72, SIN60, &
                  C1, S1, C2, S2, C3, S3, C4, S4

      INTEGER  :: IINK, JINK, JUMP, IBASE, JBASE, IGO, &
                  IA, JA, IB, JB, KB, KC, IC, JC, ID, JD, KD, IE, JE, KE, &
                  I, J, K, L, M, LA1, IJK

!DG      DATA SIN36/0.587785252292473_r8/,COS36/0.809016994374947_r8/, &
!           SIN72/0.951056516295154_r8/,COS72/0.309016994374947_r8/, &
!           SIN60/0.866025403784437_r8/
      DATA SIN36/0.587785252292473/,COS36/0.809016994374947/, &
           SIN72/0.951056516295154/,COS72/0.309016994374947/, &
           SIN60/0.866025403784437/

      M=N/IFAC
      IINK=M*INC1
      JINK=LA*INC2
      JUMP=(IFAC-1)*JINK
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF (IGO.GT.4) RETURN
      GO TO (10,50,90,130),IGO

!     CODING FOR FACTOR 2

   10 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      DO 20 L=1,LA
      I=IBASE
      J=JBASE

      DO 15 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      D(JB+J)=B(IA+I)-B(IB+I)
      I=I+INC3
      J=J+INC4
   15 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   20 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 40 K=LA1,M,LA
      KB=K+K-2
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      DO 30 L=1,LA
      I=IBASE
      J=JBASE

      DO 25 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      C(JB+J)=C1*(A(IA+I)-A(IB+I))-S1*(B(IA+I)-B(IB+I))
      D(JB+J)=S1*(A(IA+I)-A(IB+I))+C1*(B(IA+I)-B(IB+I))
      I=I+INC3
      J=J+INC4
   25 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   30 CONTINUE
      JBASE=JBASE+JUMP
   40 CONTINUE
      RETURN

!     CODING FOR FACTOR 3

   50 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      DO 60 L=1,LA
      I=IBASE
      J=JBASE

      DO 55 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IC+I))
      C(JB+J)=(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))
      C(JC+J)=(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))
      D(JB+J)=(B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I)))
      D(JC+J)=(B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I)))
      I=I+INC3
      J=J+INC4
   55 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   60 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 80 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      DO 70 L=1,LA
      I=IBASE
      J=JBASE

      DO 65 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IC+I))
      C(JB+J)= &
         C1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))) &
        -S1*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      D(JB+J)= &
         S1*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))-(SIN60*(B(IB+I)-B(IC+I)))) &
        +C1*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))+(SIN60*(A(IB+I)-A(IC+I))))
      C(JC+J)= & 
         C2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))) &
        -S2*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      D(JC+J)= &
         S2*((A(IA+I)-0.5*(A(IB+I)+A(IC+I)))+(SIN60*(B(IB+I)-B(IC+I)))) &
        +C2*((B(IA+I)-0.5*(B(IB+I)+B(IC+I)))-(SIN60*(A(IB+I)-A(IC+I))))
      I=I+INC3
      J=J+INC4
   65 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   70 CONTINUE
      JBASE=JBASE+JUMP
   80 CONTINUE
      RETURN

!     CODING FOR FACTOR 4

   90 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      ID=IC+IINK
      JD=JC+JINK
      DO 100 L=1,LA
      I=IBASE
      J=JBASE

      DO 95 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      C(JC+J)=(A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)+B(IC+I))+(B(IB+I)+B(ID+I))
      D(JC+J)=(B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I))
      C(JB+J)=(A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))
      C(JD+J)=(A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))
      D(JB+J)=(B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I))
      D(JD+J)=(B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I))
      I=I+INC3
      J=J+INC4
   95 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  100 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 120 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      DO 110 L=1,LA
      I=IBASE
      J=JBASE

      DO 105 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)+B(IC+I))+(B(IB+I)+B(ID+I))
      C(JC+J)= &
         C2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))) &
        -S2*((B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I)))
      D(JC+J)= &
         S2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))) &
        +C2*((B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I)))
      C(JB+J)= &
         C1*((A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))) &
        -S1*((B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I)))
      D(JB+J)= &
         S1*((A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))) &
        +C1*((B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I)))
      C(JD+J)= &
         C3*((A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))) &
        -S3*((B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I)))
      D(JD+J)= &
         S3*((A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))) &
        +C3*((B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  105 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  110 CONTINUE
      JBASE=JBASE+JUMP
  120 CONTINUE
      RETURN
!
!     CODING FOR FACTOR 5
!
  130 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      ID=IC+IINK
      JD=JC+JINK
      IE=ID+IINK
      JE=JD+JINK
      DO 140 L=1,LA
      I=IBASE
      J=JBASE

      DO 135 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IE+I))+(B(IC+I)+B(ID+I))
      C(JB+J)=(A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) &
       -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))
      C(JE+J)=(A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) &
       +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))
      D(JB+J)=(B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) &
       +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I)))
      D(JE+J)=(B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) &
       -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I)))
      C(JC+J)=(A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) &
       -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))
      C(JD+J)=(A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) &
       +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))
      D(JC+J)=(B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) &
       +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I)))
      D(JD+J)=(B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) &
       -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  135 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  140 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 160 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      DO 150 L=1,LA
      I=IBASE
      J=JBASE

      DO 145 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IE+I))+(B(IC+I)+B(ID+I))
      C(JB+J)= &
         C1*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) &
           -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) &
        -S1*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) &
           +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      D(JB+J)= &
         S1*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) &
           -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) &
        +C1*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) &
           +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      C(JE+J)= &
         C4*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) &
           +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) &
        -S4*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) &
           -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      D(JE+J)= &
         S4*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I))) &
           +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))) &
        +C4*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I))) &
           -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      C(JC+J)= &
         C2*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) &
           -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) &
        -S2*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) &
           +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      D(JC+J)= &
         S2*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) &
           -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) &
        +C2*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) &
           +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      C(JD+J)= &
         C3*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) &
           +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) &
        -S3*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) &
           -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      D(JD+J)= &
         S3*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I))) &
           +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))) &
        +C3*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I))) &
           -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      I=I+INC3
      J=J+INC4
  145 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  150 CONTINUE
      JBASE=JBASE+JUMP
  160 CONTINUE
      RETURN
  END SUBROUTINE
!----------------------------------------------------------------------------
!END OF THE FFT BLOCK
!---------------------------------------------------

END MODULE RANDOM_ROUTINES