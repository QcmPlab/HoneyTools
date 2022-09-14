module coordinates
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
      procedure :: new_hex
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

   !> PUBLIC NAMES API [private at bottom]

   pure function hex_norm(A) result(n)
      !! Norm of a point from its hex coordinates
      type(hex),intent(in) :: A
      integer              :: n
      n = 0.5d0 * (abs(A%q) + &
                   abs(A%r) + &
                   abs(A%s))
   end function

   pure function hex_distance(A,B) result(d)
      !! Distance of two points from their hex coordinates
      type(hex),intent(in) :: A,B
      integer              :: d
      d = hex_norm(A - B) ! overloaded subtraction
   end function

   impure elemental subroutine hex_print(A)
      !! Pretty print of hex coordinates
      type(hex),intent(in) :: A
      print*, "Hex coordinates [q,r,s]: ", A%q, A%r, A%s
   end subroutine

   !> THESE ARE PRIVATE NAMES

   pure function new_hex(q,r,s) result(self)
      !! Safe constructor for the hex type
      integer,intent(in) :: q,r,s
      type(hex)          :: self
      !> assertion on input coordinates
      call assert(q+r+s==0, "q + r + s == 0", q+r+s)
      !> initialization of hex object
      self%q = q
      self%r = r
      self%s = s
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

end module coordinates

