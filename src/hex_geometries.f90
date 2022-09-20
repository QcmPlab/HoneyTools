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
   end function

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
   end function

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
   end function

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
   end function

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
   end function

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

end module hex_geometries
