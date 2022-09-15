module xy_coordinates
   !! Defining real space 2D coordinates for honeycomb lattice sites

   use hex_coordinates
   use hex_layout

   implicit none
   private

   public :: xy, xy_tile, hex2center, hex2corner, xy_print

   integer, parameter :: N = 6 ! Number of vertices in a hexagon
   real(8), parameter :: PI = 4d0*atan(1D0) ! π to selected kind

   type xy
      real(8) :: x
      real(8) :: y
   end type

   type xy_tile
      type(xy),dimension(N) :: vertex
   end type

contains

   pure elemental function hex2corner(layout,H) result(corner)
      !! Convert hex coordinates to real 2D space
      !! [returning the xy coordinates for the hexagon corners,
      !!  as appropriatiely wrapped in a "xy_tile" derived type]
      !! Actual real-space layout has to be specified by passing
      !! a (scalar) unit_cell object.
      type(unit_cell),intent(in) :: layout
      type(hex),intent(in)       :: H
      type(xy)                   :: center
      type(xy)                   :: offset
      type(xy_tile)              :: corner
      type(hex_orientation)      :: basis
      integer                    :: i
      center = hex2center(layout,H)
      do i = 1,N
         offset = ith_corner_offset(layout,i)
         corner%vertex(i) = xy(center%x+offset%x,center%y+offset%y)
      enddo
   end function

   pure elemental function hex2center(layout,H) result(center)
      !! Convert hex coordinates to real 2D space
      !! [returning the xy coordinates of the hexagon center]
      !! Actual real-space layout has to be specified by passing
      !! a (scalar) unit_cell object.
      type(unit_cell),intent(in) :: layout
      type(hex),intent(in)       :: H
      type(xy)                   :: center
      type(hex_orientation)      :: basis
      ! Project [q,r] along unit-cell basis to get [x,y]
      basis = layout%orientation
      center%x = basis%uq(1) * H%q + basis%ur(1) * H%r ! mixed math ok
      center%y = basis%uq(2) * H%q + basis%ur(2) * H%r ! mixed math ok
      ! Rescale and recenter according to unit-cell data
      center%x = center%x * layout%size + layout%origin(1)
      center%y = center%y * layout%size + layout%origin(2)
      ! Equivalent to:
      !        ⎡x⎤   ⎡uq(1)  ur(1)⎤   ⎡q⎤
      !        ⎥ ⎥ = ⎥            ⎥ × ⎥ ⎥ × size + origin
      !        ⎣y⎦   ⎣uq(2)  ur(2)⎦   ⎣r⎦
   end function

   pure function ith_corner_offset(layout,i) result(offset)
      !! Compute the i-th offset vector connecting the hexagon center
      !! to its corners, returning a (scalar) xy coordinate
      type(unit_cell),intent(in) :: layout
      integer,intent(in)         :: i
      type(xy)                   :: offset
      real(8)                    :: angle
      angle = 2d0*PI/N * (layout%orientation%angle + i) ! mixed math ok 
      offset = xy(x=layout%size*cos(angle), y=layout%size*sin(angle))
   end function

   impure elemental subroutine xy_print(S,unit,quiet)
      !! Pretty print of xy coordinates
      type(xy),intent(in)          :: S
      integer,intent(in),optional  :: unit  !! default = $stdout
      logical,intent(in),optional  :: quiet !! default = .false.
      integer                      :: stdunit
      logical                      :: verbose
      if(present(quiet))then
         verbose = .not.quiet
      else
         verbose = .true.
      endif
      if(present(unit))then
         stdunit = unit
      else
         stdunit = 6 ! stdout
      endif
      if(verbose)then
         write(stdunit,*) "real-space coordinates [x,y]: ", S%x, S%y
      else
         write(stdunit,*) S%x, S%y
      endif
   end subroutine

end module xy_coordinates
