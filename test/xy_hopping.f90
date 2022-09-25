program test_xy_hoppings

   use honeytools
   use honeyplots
   use xy_coordinates
   use hex_layout
   use assert_m, only: assert ![pure assertion]

   implicit none

   type(xy_lattice)        :: lattice
   type(xy_site)           :: S,NN(3),NNN(6)
   type(unit_cell)         :: layout
   real(8),dimension(2)    :: e1,e2,a1,a2,a3,d1,d2,d3
   integer                 :: i

   print*
   print*,"CHECK SINGLE POINT HOPPINGS"
   print*
   ! CUSTOM lattice basis:
   ! e₁ = a₀ [ sqrt3/2 , 1/2 ] = 3/2a[1, 1/sqrt3]
   ! e₂ = a₀ [ sqrt3/2 ,-1/2 ] = 3/2a[1,-1/sqrt3]
   e1 = 3d0/2d0*[1d0, 1d0/sqrt(3d0)]
   e2 = 3d0/2d0*[1d0,-1d0/sqrt(3d0)]
   !
   ! Unit-cell displacements: nearest neighbor A-->B, B-->A
   d1= [  1d0/2d0 , sqrt(3d0)/2d0 ]
   d2= [  1d0/2d0 ,-sqrt(3d0)/2d0 ]
   d3= [ -1d0     , 0d0           ]
   !
   ! Cell displacements: next nearest-neighbor A-->A, B-->B
   a1 = d1-d3     !3/2*a[1,1/sqrt3]
   a2 = d2-d3     !3/2*a[1,-1/sqrt3]
   a3 = d1-d2     !sqrt3[0,1]
   !
   ! CUSTOM LAYOUT
   layout = unit_cell(hex_orientation(e1,e2,angle=0))
   !
   S%x = 0
   S%y = 0
   !
   S%label = "A"
   NN(1)%x = S%x + d1(1)
   NN(1)%y = S%y + d1(2)
   NN(1)%label = "B"
   NN(2)%x = S%x + d2(1)
   NN(2)%y = S%y + d2(2)
   NN(2)%label = "B"
   NN(3)%x = S%x + d3(1)
   NN(3)%y = S%y + d3(2)
   NN(3)%label = "B"
   !
   lattice = xy_lattice(site=[S,NN],size=4)
   call check_nn_hops
   !
   NNN(1)%x = S%x + a1(1)
   NNN(2)%x = S%x - a1(1)
   NNN(3)%x = S%x + a2(1)
   NNN(4)%x = S%x - a2(1)
   NNN(5)%x = S%x + a3(1)
   NNN(6)%x = S%x - a3(1)
   !
   NNN(1)%y = S%y + a1(2)
   NNN(2)%y = S%y - a1(2)
   NNN(3)%y = S%y + a2(2)
   NNN(4)%y = S%y - a2(2)
   NNN(5)%y = S%y + a3(2)
   NNN(6)%y = S%y - a3(2)
   !
   NNN%label = "A"
   !
   lattice = xy_lattice(site=[S,NN,NNN],size=10)
   call check_nn_hops
   call check_nnn_hops
   !
   S%label = "B"
   NN(1)%x = S%x - d1(1)
   NN(1)%y = S%y - d1(2)
   NN(1)%label = "A"
   NN(2)%x = S%x - d2(1)
   NN(2)%y = S%y - d2(2)
   NN(2)%label = "A"
   NN(3)%x = S%x - d3(1)
   NN(3)%y = S%y - d3(2)
   NN(3)%label = "A"
   !
   lattice = xy_lattice(site=[S,NN],size=4)
   call check_nn_hops
   !
   NNN(1)%x = S%x + a1(1)
   NNN(2)%x = S%x - a1(1)
   NNN(3)%x = S%x + a2(1)
   NNN(4)%x = S%x - a2(1)
   NNN(5)%x = S%x + a3(1)
   NNN(6)%x = S%x - a3(1)
   !
   NNN(1)%y = S%y + a1(2)
   NNN(2)%y = S%y - a1(2)
   NNN(3)%y = S%y + a2(2)
   NNN(4)%y = S%y - a2(2)
   NNN(5)%y = S%y + a3(2)
   NNN(6)%y = S%y - a3(2)
   !
   NNN%label = "B"
   !
   lattice = xy_lattice(site=[S,NN,NNN],size=10)
   call check_nn_hops
   call check_nnn_hops

   print*
   print*,"CHECK ARMCHAIR LATTICE HOPPINGS"
   print*
   layout = unit_cell(armchair)
   call check_all_lattices

   print*
   print*,"CHECK ZIGZAG LATTICE HOPPINGS"
   print*
   layout = unit_cell(zigzag)
   call check_all_lattices

contains

   subroutine check_all_lattices
      integer :: d
      do d = 2,5
         lattice = get_flake(d,layout)
         call check_nn_hops
         call check_nnn_hops
         lattice = get_triangle(d,layout)
         call check_nn_hops
         call check_nnn_hops
         lattice = get_stripe(d,d,layout)
         call check_nn_hops
         call check_nnn_hops
         lattice = get_supercell(d,d,layout)
         call check_nn_hops
         call check_nnn_hops
      enddo
   end subroutine

   subroutine check_nn_hops
      integer              :: i,j,k,l
      logical,allocatable  :: nn_mask(:,:)
      type(xy_site)        :: S,NN(3),NNN(6)
      call xy_nearest_neighbors(lattice,nn_mask)
      k = 0 ! counter
      do l = 1,lattice%size
         print*
         S = lattice%site(l)
         call xy_print(S)
         print*
         print*, "  NNs:"
         do i = 1,3
            NN(i) = xy_nn_hop(layout,S,i)
            call xy_print(NN(i),quiet=.true.)
            if(any(NN(i)==lattice%site))then
               call xy_print(NN(i),quiet=.true.)
               k = k + 1
            endif
         enddo
      enddo
      !call plot(lattice,nn_mask)
      print*
      print*, "nn-k =",k
      print*, "count =", count(nn_mask)
      if(k > count(nn_mask))then
         error stop "Found too much NNs"
      elseif(k < count(nn_mask))then
         error stop "Found too few NNs"
      endif
   end subroutine

   subroutine check_nnn_hops
      integer              :: i,j,k,l
      logical,allocatable  :: nnn_mask(:,:)
      type(xy_site)        :: S,NN(3),NNN(6)
      call xy_next_nearest_neighbors(lattice,nnn_mask)
      k = 0 ! counter
      do l = 1,lattice%size
         print*
         S = lattice%site(l)
         call xy_print(S)
         print*
         print*, "  NNNs:"
         do i = 1,6
            NNN(i) = xy_nnn_hop(layout,S,i)
            if(any(NNN(i)==lattice%site))then
               call xy_print(NNN(i),quiet=.true.)
               k = k + 1
            endif
         enddo
      enddo
      !call plot(lattice,nnn_mask=nnn_mask)
      print*
      print*, "nnn-k =",k
      print*, "count =", count(nnn_mask)
      if(k > count(nnn_mask))then
         error stop "Found too much NNNs"
      elseif(k < count(nnn_mask))then
         error stop "Found too few NNNs"
      endif
   end subroutine

end program test_xy_hoppings
