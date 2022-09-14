module hex_coordinates
   !! Defining special 3D coordinates for honeycomb lattices

   use assert_m, only : assert
   
   implicit none
   private

   public :: hex, hex_norm, hex_distance, hex_print
   public :: operator(==), operator(/=), operator(+), operator(-), operator(*)

   type hex !! cubic coordinates storage
      integer :: q, r, s
   endtype

   interface hex !! constructor overload
      procedure :: new_hex_cubic !(q,r,s)
      procedure :: new_hex_axial !(q,r)
   end interface

   interface operator(==) !! equality overload
      procedure :: eq_hex
   end interface

   interface operator(/=) !! inequality overload
      procedure :: neq_hex
   end interface

   interface operator(+)  !! addition overload
      procedure :: add_hex
   end interface

   interface operator(-)  !! subtraction overload
      procedure :: sub_hex
   end interface

   interface operator(*)  !! product overload
      procedure :: rhs_hex
      procedure :: lhs_hex
      procedure :: dot_hex
   end interface

contains

   ! PUBLIC NAMES API [private at bottom]

   pure function hex_norm(H) result(n)
      !! Taxicab norm of a point from its hex coordinates
      type(hex),intent(in) :: H
      integer              :: n
      n = (abs(H%q) + &
           abs(H%r) + &
           abs(H%s)) / 2
   end function

   pure function hex_distance(A,B) result(d)
      !! Taxicab distance of two points from their hex coordinates
      type(hex),intent(in) :: A,B
      integer              :: d
      d = hex_norm(A - B) ! overloaded subtraction
   end function

   impure elemental subroutine hex_print(H,unit,quiet)
      !! Pretty print of hex coordinates
      type(hex),intent(in)         :: H
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
         write(stdunit,*) "hex coordinate [q,r,s]: ", H%q, H%r, H%s
      else
         write(stdunit,*) H%q, H%r, H%s
      endif
   end subroutine

   ! THESE ARE PRIVATE NAMES

   pure function new_hex_cubic(q,r,s) result(H)
      !! Safe cubic constructor for the hex type
      integer,intent(in) :: q,r,s
      type(hex)          :: H
      !> featuring an assertion on input coordinates
      call assert(q+r+s==0, "q + r + s == 0", q+r+s)
      !> before the initialization of the object
      H%q = q
      H%r = r
      H%s = s
   end function

   pure function new_hex_axial(q,r) result(H)
      !! Axial cubic constructor for the hex type
      integer,intent(in)   :: q,r 
      type(hex)            :: H
      H%q = q
      H%r = r
      !> [s needs to be computed internally]
      H%s = - q - r
   end function

   pure function eq_hex(A,B) result(isequal)
      !! Equality overload for hex type
      type(hex),intent(in) :: A,B
      logical              :: isequal
      isequal = A%q == B%q .and. &
                A%r == B%r .and. &
                A%s == B%s
   end function

   pure function neq_hex(A,B) result(notequal)
      !! Inequality overload for hex type
      type(hex),intent(in) :: A,B
      logical              :: notequal
      notequal = .not.(eq_hex(A,B))
   end function

   pure function add_hex(A,B) result(C)
      !! Addition overload for hex type
      type(hex),intent(in) :: A,B
      type(hex)            :: C
      C = hex(q = A%q + B%q, &
              r = A%r + B%r, &
              s = A%s + B%s)
   end function

   pure function sub_hex(A,B) result(C)
      !! Subtraction overload for hex type
      type(hex),intent(in) :: A,B
      type(hex)            :: C
      C = hex(q = A%q - B%q, &
              r = A%r - B%r, &
              s = A%s - B%s)
   end function

   pure function rhs_hex(A,k) result(C)
      !! Right scale overload for hex type
      type(hex),intent(in) :: A
      integer,intent(in)   :: k
      type(hex)            :: C
      C = hex(q = A%q * k, &
              r = A%r * k, &
              s = A%s * k)
   end function

   pure function lhs_hex(k,B) result(C)
      !! Left scale overload for hex type
      integer,intent(in)   :: k
      type(hex),intent(in) :: B
      type(hex)            :: C
      C = hex(q = k * B%q, &
              r = k * B%r, &
              s = k * B%s)
   end function

   pure function dot_hex(A,B) result(C)
      !! Dot overload for hex type
      type(hex),intent(in) :: A,B
      integer              :: C
      C = A%q * B%q + &
          A%r * B%r + &
          A%s * B%s
   end function

end module hex_coordinates

