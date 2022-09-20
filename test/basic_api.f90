program test_basic_user_experience

   use honeytools ! We import only the top-level API here
   use honeyplots ! And the bindings to matplotlib and gnuplot
   use stdlib_strings, only: str => to_string ! Just for I/O

   implicit none

   type(xy_lattice)        :: hlattice
   type(unit_cell)         :: mylayout
   integer                 :: i,j,k
   logical,allocatable     :: NN(:,:),NNN(:,:)

   mylayout = unit_cell(orientation=armchair)
   do i = 1,6
      hlattice = get_flake(radius=i,layout=mylayout)
      call xy_nearest_neighbors(lattice=hlattice,nn_mask=NN)
      if(i==1)then
         call xy_print(hlattice)
         print*
         print*, "NN matrix:"
         do j = 1,hlattice%size
            write(*,*) (NN(j,k), k=1,hlattice%size)
         enddo
      endif
      call plot(hlattice,backend='gnuplot',set_terminal='dumb')
      call plot(hlattice,NN,figure_name='flake'//str(i)//'.svg')
   enddo

   mylayout = unit_cell(armchair,size=1,origin=[-5,-18])
   hlattice = get_supercell(5,3,mylayout)
   call xy_next_nearest_neighbors(hlattice,NNN,NN)
   call xy_print(hlattice)
   call plot(hlattice,backend='gnuplot',set_terminal='dumb')
   call plot(hlattice,NN,NNN,figure_name='supercell_armchair.svg')

   mylayout = unit_cell(zigzag,size=3,origin=[-9,-9])
   hlattice = get_supercell(3,5,mylayout)
   call xy_next_nearest_neighbors(hlattice,NNN,NN)
   call xy_print(hlattice)
   call plot(hlattice,backend='gnuplot',set_terminal='dumb')
   call plot(hlattice,NN,NNN,figure_name='supercell_zigzag.svg')

   mylayout = unit_cell(armchair)
   hlattice = get_stripe(4,5,mylayout)
   call xy_next_nearest_neighbors(hlattice,NNN,NN)
   call xy_print(hlattice)
   call plot(hlattice,backend='gnuplot',set_terminal='dumb')
   call plot(hlattice,NN,NNN,figure_name='stripe_armchair.svg')

   mylayout = unit_cell(zigzag)
   hlattice = get_stripe(7,5,mylayout)
   call xy_next_nearest_neighbors(hlattice,NNN,NN)
   call xy_print(hlattice)
   call plot(hlattice,backend='gnuplot',set_terminal='dumb')
   call plot(hlattice,NN,NNN,figure_name='stripe_zigzag.svg')

   mylayout = unit_cell(armchair)
   do i = 2,5 ! triangles of size = 1 cannot exist
      hlattice = get_triangle(i,mylayout)
      call xy_next_nearest_neighbors(hlattice,NNN,NN)
      call xy_print(hlattice)
      call plot(hlattice,backend='gnuplot',set_terminal='dumb')
      call plot(hlattice,NN,NNN,figure_name='triangle'//str(i)//'.svg')
   enddo

end program test_basic_user_experience
