program test_basic_user_experience

   use honeytools
   use stdlib_strings, only: to_string
   use stdlib_hash_32bit, only: odd_random_integer
   use hex_coordinates, only: hex
   use xy_coordinates, only: hex2lattice

   implicit none

   type(xy_lattice)        :: hlattice
   type(unit_cell)         :: mylayout
   real(8)                 :: a(5) = [1,2,5,7,9] ! lattice spacing
   real(8)                 :: o(2) ! lattice center
   integer                 :: i,j,k
   logical,allocatable     :: NN(:,:),NNN(:,:)

   do i = 1,5
      do j = 1,2
         do k = 1,2
            o = real([j,k],8)
            mylayout = unit_cell(armchair,a(i),o)
            hlattice = get_flake(i,mylayout)
         enddo
      enddo
      call xy_nearest_neighbors(hlattice,NN)
      call plot(hlattice,backend='gnuplot',set_terminal='dumb')
      call plot(hlattice,NN,figure_name='flake'//to_string(i)//'.svg')
   enddo

   hlattice = get_supercell(3,5,mylayout)
   call xy_next_nearest_neighbors(hlattice,NNN,NN)
   call plot(hlattice,backend='gnuplot',set_terminal='dumb')
   call plot(hlattice,NN,NNN,figure_name='supercell.svg')

   mylayout = unit_cell(zigzag)
   hlattice = get_stripe(3,5,mylayout)
   call xy_next_nearest_neighbors(hlattice,NNN,NN)
   call plot(hlattice,backend='gnuplot',set_terminal='dumb')
   call plot(hlattice,NN,NNN,figure_name='zigzag.svg')

   mylayout = unit_cell(armchair)
   hlattice = get_stripe(3,5,mylayout)
   call xy_next_nearest_neighbors(hlattice,NNN,NN)
   call plot(hlattice,backend='gnuplot',set_terminal='dumb')
   call plot(hlattice,NN,NNN,figure_name='armchair.svg')

   do i = 2,5 ! triangles of size 1 are cannot exist
      hlattice = get_triangle(i,mylayout)
      call xy_next_nearest_neighbors(hlattice,NNN,NN)
      call plot(hlattice,backend='gnuplot',set_terminal='dumb')
      call plot(hlattice,NN,NNN,figure_name='triangle'//to_string(i)//'.svg')
   enddo

end program test_basic_user_experience
