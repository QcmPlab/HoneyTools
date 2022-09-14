module hex_layout
   !! Defining hexagonal unit-cells

   implicit none
   private

   public :: zigzag, armchair, unit_cell, print_unit_cell

   type hex_orientation !! lattice-orientation data storage
      real(8), dimension(2,2) :: F !! Forward rotation matrix
      real(8), dimension(2,2) :: B !! Backward rotation matrix
      real(8)                 :: angle !! in units of 60°
   end type

   interface operator(==) ! equality overload
      procedure :: orientation_equality 
   end interface

   ! Zigzag Forward Rotation Matrix:
   real(8),dimension(2,2), parameter :: ZF = reshape([sqrt(3d0), sqrt(3d0)/2d0, &
                                                         0d0,       3d0/2d0   ],[2,2])
   ! Zigzag Backward Rotation Matrix:
   real(8),dimension(2,2), parameter :: ZB = reshape([sqrt(3d0)/3d0, -1/3d0, &
                                                         0d0,       2d0/3d0   ],[2,2])
   ! Armchair Forward Rotation Matrix:
   real(8),dimension(2,2), parameter :: AF = reshape([sqrt(3d0), sqrt(3d0)/2d0, &
                                                         0d0,       3d0/2d0   ],[2,2])
   ! Armchair Backward Rotation Matrix: 
   real(8),dimension(2,2), parameter :: AB = reshape([sqrt(3d0)/3d0, -1/3d0, &
                                                         0d0,       2d0/3d0   ],[2,2])                                            

   type(hex_orientation), parameter :: zigzag = hex_orientation(ZF,ZB,0.5d0)
   type(hex_orientation), parameter :: armchair = hex_orientation(AF,AB,0d0)

   type unit_cell
      !! Fields:
      !! • orientation :: armchair or zigzag, mandatory to construct
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
      isequal = all(a%F == b%F)
      isequal = isequal .and. all(a%B == b%B)
      isequal = isequal .and. a%angle == b%angle
   end function

end module hex_layout
