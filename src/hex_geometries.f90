module hex_geometries
   !! Provide common honeycomb geometries, as dynamic type(hex) arrays

   use hex_coordinates
   use hex_layout
   use assert_m, only : assert ![pure assertion for triangle]

   implicit none
   private

   public :: hex_insert ! utility to grow hex arrays
   public :: hex_supercell, hex_triangle, hex_flake
   public :: hex_armchair_stripe, hex_zigzag_stripe
   public :: hex_line ! to "draw" lines in hex space

contains

   ! PUBLIC API [private at bottom]

   pure function hex_supercell(rows,cols) result(hexagons)
      !! Build a honeycomb supercell (parallelogram)
      integer,intent(in)   :: rows,cols
      type(hex)            :: hexagons(rows*cols)
      integer              :: i,j,k
      k = 0
      do i = 1,rows
         do j = 1,cols
            k = k + 1
            hexagons(k) = hex(q=i,r=j)
         enddo
      enddo
   end function hex_supercell

   pure function hex_triangle(size) result(hexagons)
      !! Build a triangle-shaped honeycomb flake
      integer,intent(in)      :: size
      type(hex),allocatable   :: hexagons(:)
      integer                 :: i,j
      call assert(size>1,"triangle size > 1",size)
      do i = 0,size
         do j = 0,size-i
            call hex_insert(hexagons,hex(i,j))
         enddo
      enddo
   end function hex_triangle

   pure function hex_flake(radius) result(hexagons)
      !! Build a hexagon-shaped honeycomb flake
      integer,intent(in)      :: radius
      type(hex),allocatable   :: hexagons(:)
      integer                 :: i,j
      do i = -radius,+radius
         do j = max(-radius,-i-radius),min(radius,-i+radius)
            call hex_insert(hexagons,hex(i,j))
         enddo
      enddo
   end function hex_flake

   pure function hex_armchair_stripe(height,width) result(hexagons)
      !! Build a armchair-on-top honeycomb stripe/ribbon
      !! CONVERSION TO XY SITES REQUIRES ARMCHAIR LAYOUT
      integer,intent(in)      :: height,width
      type(hex),allocatable   :: hexagons(:)
      integer                 :: i,j,offset
      do i = 1,width
         offset = floor(i/2.d0)
         do j = 1-offset,height-offset
            call hex_insert(hexagons,hex(i,j))
         enddo
      enddo
   end function hex_armchair_stripe

   pure function hex_zigzag_stripe(height,width) result(hexagons)
      !! Build a zigzag-on-top honeycomb stripe/ribbon
      !! CONVERSION TO XY SITES REQUIRES ZIGZAG LAYOUT
      integer,intent(in)      :: height,width
      type(hex),allocatable   :: hexagons(:)
      integer                 :: i,j,offset
      do i = 1,height
         offset = floor(i/2.d0)
         do j = 1-offset,width-offset
            call hex_insert(hexagons,hex(i,j))
         enddo
      enddo
   end function hex_zigzag_stripe

   pure function hex_line(A,B) result(line)
      !! Draw a line between A and B, in hex space
      type(hex),intent(in)  :: A,B
      type(hex),allocatable :: line(:)
      real(8),dimension(3)  :: lntrp
      integer,dimension(3)  :: ai,bi
      real(8),dimension(3)  :: ar,br
      real(8)               :: step
      integer               :: dist,i
      dist = hex_distance(A,B)
      step = 1d0 / max(dist,1) ! handle A==B :)
      do i = 0,dist
         ! Unpack
         ai = [A%q,A%r,A%s]
         bi = [B%q,B%r,B%s]
         ! Cast
         ar = real(ai,8)
         br = real(bi,8)
         ! Nudge
         ar(1:2) = ar(1:2) + 1d-6
         ar(3)   = ar(3)   - 2d-6
         br(1:2) = br(1:2) + 1d-6
         br(3)   = br(3)   - 2d-6
         ! Interpolate
         lntrp = linear_interpolation(ar,br,step*i)
         ! Grow the vect
         call hex_insert(line,hex_round(lntrp))
      enddo
   end function hex_line

   pure subroutine hex_insert(vec,val)
      !! Utility to grow type(hex) arrays, it is a
      !! poor man implementation of a dynamic array
      !! insertion, a là std::vector (but with no
      !! preallocation and smart doubling...)
      type(hex),intent(inout),allocatable :: vec(:)
      type(hex),intent(in)                :: val
      type(hex),allocatable               :: tmp(:)
      integer                             :: len
      if(allocated(vec))then
         len = size(vec)
         allocate(tmp(len+1))
         tmp(:len) = vec
         call move_alloc(tmp,vec)
         len = len + 1
      else
         len = 1
         allocate(vec(len))
      end if
      ! Insert val at back
      vec(len) = val
   end subroutine hex_insert

   ! THESE ARE PRIVATE NAMES

   pure elemental function linear_interpolation(a,b,t) result(l)
      !! Linear interpolation from a to b, with step t
      !! a + (b - a) * t = a * (1-t) + b * t
      !! for better floating-point precision
      real(8),intent(in)   :: a,b,t
      real(8)              :: l
      l = a * (1-t) + b * t
   end function linear_interpolation

   pure function hex_round(xyz) result(qrs)
      !! Round a triplet of reals to a proper hex object
      !! > this needs to preserve the q+r+s==0 condition
      real(8),intent(in)   :: xyz(3)
      type(hex)            :: qrs
      real(8)              :: x,y,z
      integer              :: q,r,s
      real(8)              :: dq,dr,ds
      ! Unpack
      x = xyz(1)
      y = xyz(2)
      z = xyz(3)
      ! Round xyz
      q = nint(x)
      r = nint(y)
      s = nint(z)
      ! Eval diffs
      dq = abs(x-q)
      dr = abs(x-r)
      ds = abs(x-s)
      ! Reset bigger diff
      if(dq > dr .and. dq > ds)then
         q = -r-s
      elseif(dr > ds)then
         r = -q-s
      else
         s = -q-r
      endif
      ! Repack
      qrs = hex(q,r,s) ! internal assertion :)
   end function hex_round

end module hex_geometries
