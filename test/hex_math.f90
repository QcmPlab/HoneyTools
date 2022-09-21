program test_hex_math

   use hex_coordinates
   use hex_neighbors
   use assert_m, only: assert

   implicit none

   type(hex)   :: a,b,c
   integer     :: d

   print*
   print*, "HEX MATH [unit-test]" 

   a = hex(3,-7, 4)
   b = hex(1,-3, 2)
   c = hex(4,-10,6)

   call assert(c==a+b,"hex + hex")

   a = hex( 1,-3, 2)
   b = hex( 3,-7, 4)
   c = hex(-2, 4,-2)

   call assert(c==a-b,"hex - hex")

   a = hex(0, 0, 0)
   b = hex(3,-7, 4)
   d = hex_distance(a,b)

   call assert(d==7,"hex distance")

   a = hex(1,-3, 2)
   b = hex(1,-2, 1)
   c = hex_hop(b,2)

   call assert(a==c,"hex neighbor")
   
   print*, "> passed."
   print*

end program test_hex_math