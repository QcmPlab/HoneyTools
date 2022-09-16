module hex_layout
   !! Defining hexagonal unit-cells

   implicit none
   private

   public :: zigzag, armchair, unit_cell, print_unit_cell ! exp to top level
   public :: hex_orientation, operator(==) ! not to be exported to top level

   type hex_orientation !! lattice-orientation data storage
      real(8), dimension(2) :: uq    !! 1st unit-vector
      real(8), dimension(2) :: ur    !! 2nd unit-vector
      real(8)               :: angle !! in units of 60°
   end type

   interface operator(==) ! equality overload
      procedure :: orientation_equality 
   end interface
      
   ! Armchair unit-vectors:
   real(8),dimension(2),parameter :: aq = [      3d0/2d0, sqrt(3d0)/2d0]
   real(8),dimension(2),parameter :: ar = [          0d0,     sqrt(3d0)]
   ! Zig-Zag unit-vectors:
   real(8),dimension(2),parameter :: zq = [    sqrt(3d0),           0d0]
   real(8),dimension(2),parameter :: zr = [sqrt(3d0)/2d0,       3d0/2d0]
   ! Predefined orientations:
   type(hex_orientation), parameter :: armchair = hex_orientation(aq,ar,0d0)
   type(hex_orientation), parameter :: zigzag = hex_orientation(zq,zr,0.5d0)

   type unit_cell
      !! Fields:
      !! • orientation :: armchair or zigzag, mandatory
      !! • size        :: lattice parameter, optional, defaults to 1d0
      !! • origin      :: center of the cell, optional, defaults to [0d0,0d0]
      type(hex_orientation) :: orientation
      real(8)               :: size = 1d0 ! put dimension(2) to enable anysotropy?
      real(8),dimension(2)  :: origin = [0d0,0d0]
   end type

contains

   impure elemental subroutine print_unit_cell(C,unit)
      !! Pretty print of hex coordinates
      type(unit_cell),intent(in)   :: C
      integer,intent(in),optional  :: unit  !! default = $stdout
      integer                      :: stdunit
      if(present(unit))then
         stdunit = unit
      else
         stdunit = 6 ! stdout
      endif
      write(stdunit,*) "HEXAGONAL UNIT-CELL"
      if(C%orientation == armchair)then
         write(stdunit,*) "Orientation: ARMCHAIR"
      elseif(C%orientation == zigzag)then
         write(stdunit,*) "Orientation: ZIGZAG"
      else
         write(stdunit,*) "Orientation: UNKNOWN"
      end if
      write(stdunit,*) "lattice parameter = ", C%size
      write(stdunit,*) "located at [", C%origin(1), C%origin(2), "]"
      write(stdunit,*) ""
   end subroutine

   pure function orientation_equality(a,b) result(isequal)
      !! Overload of == operator for hex_orientation type
      type(hex_orientation),intent(in) :: a,b
      logical                          :: isequal
      isequal = all(abs(a%uq - b%uq) < 1d-12)
      isequal = isequal .and. all(abs(a%ur - b%ur) < 1d-12)
      isequal = isequal .and. (a%angle - b%angle) < 1d-12
   end function

end module hex_layout
