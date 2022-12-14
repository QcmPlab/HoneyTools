program development_log

   use hex_coordinates
   use hex_layout
   use hex_neighbors
   use xy_coordinates
   use xy_neighbors
   use honeyplots
   use assert_m, only : assert

   implicit none

   type(hex)                :: a,b,c
   type(hex),dimension(6)   :: neighborhood
   type(unit_cell)          :: u,v
   type(xy)                 :: center
   type(xy),dimension(6)    :: centers
   type(xy_lattice)         :: vertices
   type(xy_site)            :: subvector(6)
   type(xy_lattice)         :: hexagons(6)
   type(xy_lattice)         :: lattice,reticolo
   type(xy_lattice)         :: sublattice
   integer                  :: i,j,k,l
   integer,allocatable      :: H(:,:),indices(:)
   real(8),allocatable      :: shell_table(:,:)
   real(8),allocatable      :: distance_set(:)
   logical,allocatable      :: NN_(:,:),NN(:,:)
   logical,allocatable      :: NNN(:,:)

   print*
   print*, "DEVELOPMENT LOG [regression testsuite]"
   print*

   a = hex(1,2,-3)   ! q+r+s==0 asserted internally
   b = hex(q=1,r=-2) ! s = 1 is computed internally

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

   print*, ""
   print*, "Neighbors of hex a are [q,r,s]:"
   do i = -2, 3
      c = hex_hop(a,i)
      open(unit=7,action='write')
      call hex_print(c,quiet=.true.,unit=7)
      call hex_print(c,quiet=.true.)
   end do

   print*, ""
   print*, "Let's recompute them all at once:"
   neighborhood = hex_nearest(a)
   call hex_print(neighborhood,quiet=.true.)
   open(unit=8,action='write')
   call hex_print(neighborhood,quiet=.true.,unit=8)

   u = unit_cell(orientation=armchair,size=1)
   v = unit_cell(zigzag,origin=[-1,1])

   call u%print
   call v%print
   ! All together [elemental]
   open(unit=9,action='write')
   call print_unit_cell([u,v],unit=9)

   print*, ""
   print*, "Neighbors of hex a are [x,y]:"
   do i = 1,size(neighborhood)
      center = hex2center(v,neighborhood(i))
      call xy_print(center)
   enddo

   print*, ""
   print*, "Let's re-convert them all at once:"
   centers = hex2center(v,neighborhood)
   call xy_print(centers,quiet=.true.)
   open(unit=10,action='write')
   call xy_print(centers,quiet=.true.,unit=10)

   print*, ""
   print*, "The vertices defining hex a are:"
   vertices = hex2corner(v,a)
   do i = 1,6
      call xy_print(vertices%site(i),unit=10,quiet=.true.)
   enddo

   print*, ""
   print*, "Let's compute all-at-once the vertices for the neighborhood of a"
   hexagons = hex2corner(v,neighborhood)
   do i = 1,6
      call xy_print(hexagons(i),quiet=.true.)
      open(unit=10+i,action='write')
      call xy_print(hexagons(i),quiet=.true.,unit=10+i)
   enddo

   print*, ""
   print*, "Same thing but with unique entries!"
   print*, ""
   print*, "Sublattice 'A'"
   subvector = hex2site(v,neighborhood,label="A")
   call xy_print(subvector,quiet=.true.)
   open(unit=17,action='write')
   call xy_print(subvector,quiet=.true.,unit=17)
   print*, ""
   print*, "Sublattice 'B'"
   subvector = hex2site(v,neighborhood,label="B")
   call xy_print(subvector,quiet=.true.)
   open(unit=18,action='write')
   call xy_print(subvector,quiet=.true.,unit=18)

   print*
   print*, "Oh no, the sublattices actually miss some"
   print*, "site, since we are at finite size!!! I.e."
   print*, "the border effects disrupt the assumption"
   print*, "that each hexagon correspons to two sites"
   print*, "since connectivity is greatly suppressed "
   print*, "therein. Hence we need an alternative way"
   print*, "to build build a complete set of sites.  "
   print*
   lattice = xy_ordered_union(hexagons(1),hexagons(2))
   do i = 3,6
      lattice = xy_ordered_union(lattice,hexagons(i))
   enddo
   ! All together mighty wrapper:
   reticolo = hex2lattice(v,neighborhood)
   call assert(lattice==reticolo,'test equality for lattices')
   reticolo = hex2lattice(u,neighborhood)
   call assert(lattice/=reticolo,'test inequality for lattices')

   ! I/O tests with new method
   print*, "FULL LATTICE"
   call xy_print(lattice)
   print*
   print*, "A SUBLATTICE"
   call xy_print(get_sublattice(lattice,"A"))
   print*
   print*, "B SUBLATTICE"
   call xy_print(get_sublattice(lattice,"B"))
   open(unit=19,action='write')
   call xy_print(get_sublattice(lattice,"A"),quiet=.true.,unit=19)
   open(unit=20,action='write')
   call xy_print(get_sublattice(lattice,"B"),quiet=.true.,unit=20)
   open(unit=21,action='write')
   call xy_print(lattice,quiet=.true.,unit=21)

   ! Fill an Hamiltonian with staggered onsite energies
   L = lattice%size
   allocate(H(L,L)); H=0
   ! Sublattice "A"
   sublattice = get_sublattice(lattice,"A")
   indices = sublattice%site%key
   do i = 1,size(indices)
      H(indices(i),indices(i)) = + 1
   enddo
   ! Sublattice "B"
   sublattice = get_sublattice(lattice,"B")
   indices = sublattice%site%key
   do i = 1,size(indices)
      H(indices(i),indices(i)) = - 1
   enddo
   open(unit=22,action='write')
   do i = 1,size(H,1)
      write(22,*) (H(i,j), j=1,size(H,1))
   enddo

   print*, ""
   print*, "Plotting neighborhood of hex a..."
   call plot(v,neighborhood,backend="pyplot",figure_name='pyflower.svg')
   call plot(v,neighborhood,backend="gnuplot",set_terminal='svg',figure_name='gnuflower.svg')
   call plot(v,neighborhood,backend="gnuplot",set_terminal='dumb')
   !call plot(v,neighborhood,backend="gnuplot") ! this would be a problem in CI
   !call plot(v,neighborhood,backend="pyplot")  ! this would be a problem in CI
   !call plot(u,neighborhood,backend="gnuplot") ! this would be a problem in CI
   !call plot(u,neighborhood,backend="pyplot")  ! this would be a problem in CI
   ! THIS HAS TO BE TESTED MUCH MORE CAREFULLY TO ASSURE GOOD COVERAGE
   call plot(u,neighborhood,backend='matlab') ! would skip due to <UNKNOWN BACKEND>
   call plot(u,neighborhood,figure_name='pyflower.svg',script_name='hex_test.py') ! auto pyplot!
   call plot(u,neighborhood,backend="gnuplot",set_terminal='png',figure_name='gnuflower.png')
   call plot(u,neighborhood,backend="gnuplot",set_terminal='dumb',script_name='hex_test.gp')

   ! Rusty test for xy_neighboors
   call xy_shells(lattice,shell_table,distance_set)
   call xy_nearest_neighbors(lattice,NN)
   call xy_next_nearest_neighbors(lattice,NNN,NN_)
   call assert(all(NN.eqv.NN_),"Consistent value for NN mask")
   print*, "NEAREST NEIGHBORS"
   do i = 1, lattice%size
      write(*,*) (NN(i,j), j = 1, lattice%size)
   enddo
   print*
   print*, "NEXT-NEAREST NEIGHBORS"
   do i = 1, lattice%size
      write(*,*) (NNN(i,j), j = 1, lattice%size)
   enddo
   print*
   call plot(lattice,backend='matlab')  ! would skip due to <UNKNOWN BACKEND>
   call plot(lattice,backend='gnuplot',figure_name='gnuflake.svg',set_terminal='svg')
   call plot(lattice,NN,script_name='xy_test.py',figure_name='pyflake.svg')
   call plot(lattice,NN,NNN,figure_name='pyball.svg')
   call plot(lattice,backend='gnuplot',set_terminal='dumb',script_name='xy_test.gp')

   print*
   print*, "DEVELOPMENT LOG [> passed!]"
   print*

end program development_log
