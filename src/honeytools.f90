module honeytools
   !! Library API

   use hex_coordinates
   use hex_layout
   use hex_neighbors
   use xy_coordinates

   implicit none
   private

   public :: say_hello

contains

   subroutine say_hello
      print *, "Hello, honeytools!"
   end subroutine say_hello


end module honeytools