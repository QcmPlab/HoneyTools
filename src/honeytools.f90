module honeytools

   use hex_coordinates
   use hex_neighbors

   implicit none
   private

   public :: say_hello, hex, hex_norm, hex_distance, hex_print, hex_hop, hex_nearest
   public :: operator(==), operator(/=), operator(+), operator(-), operator(*)

contains

   subroutine say_hello
      print *, "Hello, honeytools!"
   end subroutine say_hello


end module honeytools