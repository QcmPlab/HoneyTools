module honeytools

   use coordinates

   implicit none
   private

   public :: say_hello, hex, hex_norm, hex_distance, hex_print
   public :: operator(==), operator(/=), operator(+), operator(-), operator(*)

contains

   subroutine say_hello
      print *, "Hello, honeytools!"
   end subroutine say_hello


end module honeytools