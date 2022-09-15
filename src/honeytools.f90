module honeytools
   !! Library API

   use hex_coordinates
   use hex_layout
   use hex_neighbors
   use xy_coordinates

   implicit none
   private

   public :: say_hello, hex, hex_norm, hex_distance, hex_print, hex_hop, hex_nearest
   public :: operator(==), operator(/=), operator(+), operator(-), operator(*)
   public :: zigzag, armchair, unit_cell, print_unit_cell  ! lattice layouts
   public :: hex2center, hex2corner, xy, xy_tile, xy_print ! real-space coordinates

contains

   subroutine say_hello
      print *, "Hello, honeytools!"
   end subroutine say_hello


end module honeytools