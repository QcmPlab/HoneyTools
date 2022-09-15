program check_all
   use honeytools
   use assert_m, only : assert
   implicit none

   type(hex)                :: a,b,c
   type(hex),dimension(6)   :: neighborhood
   type(unit_cell)          :: u,v
   type(xy)                 :: center
   type(xy),dimension(6)    :: centers
   type(xy_tile)            :: vertices
   type(xy_tile)            :: many_vertices(6)
   integer                  :: i

   call say_hello()

   a = hex(1,2) ! s = -3 is computed internally
   b = hex(q=1,r=-2,s=1)

   call hex_print([a,b])

   call assert(a == a,"a == a")
   call assert(b == b,"b == b")

   print*, a==a, "(a == a)"
   print*, a==b, "(a == b)"
   print*, a/=a, "(a != a)"
   print*, a/=b, "(a != b)"

   c = a + b
   c = a - b

   print*, hex_distance(a,b), " = dist(a-b)"
   print*, hex_norm(c), "= |a-b|"

   i = a * b

   print*, i, "= <a,b> = c"

   c = i * c

   print*, hex_norm(c), "= |i*c| = d"

   c = c * i

   print*, hex_norm(c), "= |i*d|"

   print*, "Neighbors of hex a are [q,r,s]:"
   do i = -2, 3
      c = hex_hop(a,i)
      open(unit=7,action='write')
      call hex_print(c,quiet=.true.,unit=7)
      call hex_print(c,quiet=.true.)
   end do

   print*, "Let's recompute them all at once:"
   neighborhood = hex_nearest(a)
   call hex_print(neighborhood,quiet=.true.)
   open(unit=8,action='write')
   call hex_print(neighborhood,quiet=.true.,unit=8)

   u = unit_cell(orientation=armchair,size=1)
   v = unit_cell(zigzag,origin=[-1,1])

   call print_unit_cell([u,v])
   open(unit=9,action='write')
   call print_unit_cell([u,v],unit=9)

   print*, "Neighbors of hex a are [x,y]:"
   do i = 1,size(neighborhood)
      center = hex2center(u,neighborhood(i))
      call xy_print(center,quiet=.true.)
   enddo
   print*, "Let's re-convert them all at once:"
   centers = hex2center(u,neighborhood)
   call xy_print(centers,quiet=.true.)
   open(unit=10,action='write')
   call xy_print(centers,quiet=.true.,unit=10)

   print*, "The vertices defining hex a are:"
   vertices = hex2corner(u,a)
   do i = 1,6
      call xy_print(vertices%vertex(i))
   enddo

   print*, "Let's compute all-at-once the vertices for the neighborhood of a"
   many_vertices = hex2corner(u,neighborhood)
   do i = 1,6
      call xy_print(many_vertices%vertex(i),quiet=.true.)
   enddo

end program check_all
