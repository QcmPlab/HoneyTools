program test_hex_math

   use hex_coordinates
   use hex_neighbors
   use hex_geometries
   use hex_layout
   use honeyplots
   use assert_m, only: assert

   implicit none

   type(hex)   :: a,b,c
   integer     :: d
   type(hex)   :: line(6)
   type(hex)   :: rule(6)

   print*
   print*, "HEX MATH [unit-test]" 
   print*

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
   
   a = hex(0, 0, 0)
   b = hex(1,-5, 4)
   
   rule = [a, hex(0, -1, 1), hex(0, -2, 2), hex(1, -3, 2), hex(1, -4, 3), b]
   line = hex_line(a,b)

   print*, "RULE:"
   call hex_print(rule)
   call hex_plot(unit_cell(armchair),rule,"gnuplot",set_terminal="dumb")
   call hex_plot(unit_cell(armchair),rule,figure_name="rule.svg")
   print*, "LINE:"
   call hex_print(line)
   call hex_plot(unit_cell(armchair),line,"gnuplot",set_terminal="dumb")
   call hex_plot(unit_cell(armchair),line,figure_name="line.svg")

   call assert(all(rule==hex_line(a,b)),"line draw")

   print*
   print*, "> passed."
   print*

end program test_hex_math