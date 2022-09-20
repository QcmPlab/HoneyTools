program test_advanced_interface

   use stdlib_hash_32bit, only: odd_random_integer
   use hex_coordinates
   use hex_layout
   use hex_geometries
   use xy_coordinates
   use xy_neighbors
   use honeyplots, only: plot

   implicit none

   type(xy_lattice)        :: hlattice
   type(unit_cell)         :: mylayout
   logical,allocatable     :: NN(:,:),NNN(:,:)

   ! FINALLY SPECIAL HOLED FLAKE!
   hlattice = holed_flake(10)
   call xy_next_nearest_neighbors(hlattice,NNN,NN)
   call plot(hlattice,backend='gnuplot',set_terminal='dumb')
   call plot(hlattice,NN,figure_name='holed1.svg')
   call plot(hlattice,NN,NNN,figure_name='holed2.svg')

contains

   impure function holed_flake(radius) result(lattice)
      !! Build a hexagon-shaped honeycomb flake
      integer,intent(in)            :: radius
      type(xy_lattice)              :: lattice
      type(hex),allocatable         :: hexagons(:)
      integer                       :: i,j
      integer(4)                    :: k ! Needs to b a int32
      do i = -radius,+radius
         do j = max(-radius,-i-radius),min(radius,-i+radius)
            call odd_random_integer(k)
            if(mod(k,3)/=0)then
               call push_back(hexagons,hex(i,j))
            endif
         enddo
      enddo
      mylayout = unit_cell(zigzag)
      lattice = hex2lattice(mylayout,hexagons)
   end function

end program test_advanced_interface
