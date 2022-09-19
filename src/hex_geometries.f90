module hex_geometries
   !! Provide constructor for common honeycomb structures

   use hex_coordinates
   use hex_layout
   use assert_m, only : assert ![pure assertion for triangle]

   implicit none
   private

   public :: supercell, triangle, hex_flake, armchair_stripe, zigzag_stripe, push_back

contains

   ! PUBLIC API [private at bottom]

   pure function supercell(rows,cols) result(hexagons)
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
   end function

   pure function triangle(size) result(hexagons)
      !! Build a triangle-shaped honeycomb flake
      integer,intent(in)      :: size
      type(hex),allocatable   :: hexagons(:)
      integer                 :: i,j
      call assert(size>1,"triangle size > 1",size)
      do i = 0,size
         do j = 0,size-i
            call push_back(hexagons,hex(i,j))
         enddo
      enddo
   end function

   pure function hex_flake(radius) result(hexagons)
      !! Build a hexagon-shaped honeycomb flake
      integer,intent(in)      :: radius
      type(hex),allocatable   :: hexagons(:)
      integer                 :: i,j
      do i = -radius,+radius
         do j = max(-radius,-i-radius),min(radius,-i+radius)
            call push_back(hexagons,hex(i,j))
         enddo
      enddo
   end function

   pure function armchair_stripe(height,width) result(hexagons)
      !! Build a armchair-on-top honeycomb stripe/ribbon
      !! CONVERSION TO XY SITES REQUIRES ARMCHAIR LAYOUT
      integer,intent(in)      :: height,width
      type(hex),allocatable   :: hexagons(:)
      integer                 :: i,j,offset
      do i = 1,width
         offset = floor(i/2.d0)
         do j = 1-offset,height-offset
            call push_back(hexagons,hex(i,j))
         enddo
      enddo
   end function

   pure function zigzag_stripe(height,width) result(hexagons)
      !! Build a zigzag-on-top honeycomb stripe/ribbon
      !! CONVERSION TO XY SITES REQUIRES ZIGZAG LAYOUT
      integer,intent(in)      :: height,width
      type(hex),allocatable   :: hexagons(:)
      integer                 :: i,j,offset
      do i = 1,height
         offset = floor(i/2.d0)
         do j = 1-offset,width-offset
            call push_back(hexagons,hex(i,j))
         enddo
      enddo
   end function

   ! PRIVATE NAMES

   pure subroutine push_back(vec,val)
      !! Poor man implementation of a dynamic
      !! array, a là std::vector (but without
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
      !PUSH val at the BACK
      vec(len) = val
   end subroutine push_back

end module hex_geometries
