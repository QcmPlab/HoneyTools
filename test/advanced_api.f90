program test_advanced_interface

   use hex_coordinates
   use hex_layout
   use hex_geometries
   use xy_coordinates
   use xy_neighbors
   use honeyplots, only: plot

   implicit none

   type(xy_lattice)        :: lattice
   type(unit_cell)         :: mycell
   logical,allocatable     :: NN(:,:),NNN(:,:)
   real(8)                 :: e1(2),e2(2)
   type(hex_orientation)   :: mybasis

   ! Custom lattice unit vectors
   e1 = 3d0/2d0*[1d0, 1d0/sqrt(3d0)]
   e2 = 3d0/2d0*[1d0,-1d0/sqrt(3d0)]

   ! Define the hex_orientation object
   mybasis = hex_orientation(uq=e1,ur=e2,angle=1d0)
   print*, "Is this the default armchair?", mybasis == armchair
   print*, "Is this the default zigzag?", mybasis == zigzag

   ! Init the unit-cell with a custom lattice spacing
   mycell = unit_cell(mybasis,size=10,origin=[0,0])

   ! FINALLY SPECIAL HOLED FLAKE!
   lattice = holed_flake(10,mycell)
   call xy_next_nearest_neighbors(lattice,NNN,NN)
   call plot(lattice,backend='gnuplot',set_terminal='dumb')
   call plot(lattice,NN,figure_name='holed1.svg')
   call plot(lattice,NN,NNN,figure_name='holed2.svg')

contains

   impure function holed_flake(radius,layout) result(flake)
      !! Build a hexagon-shaped honeycomb flake
      integer,intent(in)            :: radius
      type(unit_cell),intent(in)    :: layout
      type(xy_lattice)              :: flake
      type(hex),allocatable         :: hexagons(:)
      integer                       :: i,j,k
      real(8)                       :: u
      do i = -radius,+radius
         do j = max(-radius,-i-radius),min(radius,-i+radius)
            k = randi(i,j)
            if(mod(k,2)/=0)then
               call hex_insert(hexagons,hex(i,j))
            endif
         enddo
      enddo
      flake = hex2lattice(mycell,hexagons)
   end function

   !> Generate random integer in {p,p+1,…,q}
   impure integer function randi(p,q)
      integer,intent(in)   :: p,q
      real(8)              :: r
      call random_number(r)
      randi = p + floor((q+1-p)*r)
   end function

end program test_advanced_interface
