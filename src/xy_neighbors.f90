module xy_neighbors
   !! Defining all neighbors shells for given xy lattices

   use xy_coordinates
   use hex_layout
   use stdlib_sorting, only: sort !=> ord_sort (impure)

   implicit none
   private

   public xy_nearest_neighbors, xy_next_nearest_neighbors
   public xy_nn_hop, xy_nnn_hop, xy_shells

   integer, parameter :: N = 6 ! Number of vertices in a hexagon
   real(8), parameter :: PI = 4d0*atan(1d0) ! π to selected kind

contains

   ! PUBLIC API [private at bottom]

   pure function xy_nn_hop(layout,S,i) result(Ni)
      !! Return the nearest neighbor of a lattice site, by hopping in
      !! the i-th direction. You can feed any i ∈ ℤ, but you can get
      !! only 3 inequivalent neighbors, depending on the label of the
      !! given site: "A" and "B" activate two different suitable sets
      !! of hopping directions. A lattice layout is required to build
      !! the concrete hopping vectors.
      type(unit_cell),intent(in) :: layout
      type(xy_site),intent(in)   :: S
      integer,intent(in)         :: i
      type(xy_site)              :: Ni
      type(xy)                   :: Oi
      select case (S%label)
       case("A")
         Oi = ith_A_offset(layout,i)
         Ni % label = "B"
       case("B")
         Oi = ith_B_offset(layout,i)
         Ni % label = "A"
      end select
      Ni % x = S % x + Oi % x
      Ni % y = S % y + Oi % y
   end function

   pure function xy_nnn_hop(layout,S,i) result(Ni)
      !! Return the i-th next-nearest neighbor of a lattice site, by
      !! taking two xy_nn_hops in the suitable direction.
      !! You can feed any i ∈ ℤ, but you can get only 6 inequivalent
      !! neighbors, depending on the label of the given site:
      !! "A" and "B" activate two different suitable sets of NN hops,
      !! A lattice layout is required to build the concrete hoppings.
      type(unit_cell),intent(in) :: layout
      type(xy_site),intent(in)   :: S
      integer,intent(in)         :: i
      type(xy_site)              :: Ni
      type(xy_site)              :: NN,tmp
      type(xy_site)              :: NNN(2)
      integer                    :: j,counter
      NN = xy_nn_hop(layout,S,i)
      counter = 0
      do j = 1,3
         tmp = xy_nn_hop(layout,NN,j+i-1)
         if(tmp/=S)then
            counter = counter + 1
            NNN(counter) = tmp
         endif
      enddo
      if(modulo(i,2)==1)then
         !  for i = ...,1,3,5,...
         NI = NNN(1)
      else
         !  for i = ...,2,4,6,...
         NI = NNN(2)
      endif
   end function

   pure subroutine xy_nearest_neighbors(lattice,nn_mask)
      !! Build a mask for the nearest neighbors of a given
      !! lattice. It is a NxN matrix, with N the number of
      !! sites in the lattice, stocking .true. values only
      !! for the pairs of sites linked by a "NN-bond".
      !! It calls internally xy_shells, for all inter-site
      !! distances, so if you need more than NNs consider
      !! calling xy_shells directly. Note that we provide
      !! a similar subroutine for next-nearest-neighbors,
      !! which optionally gives also the NNs, so if just
      !! two shells are needed you should call that.
      !! > xy_next_nearest_neighbors(lattice,nnn,[nn])
      type(xy_lattice),intent(in)      :: lattice
      logical,allocatable,intent(out)  :: nn_mask(:,:)
      real(8),allocatable              :: shell_table(:,:)
      real(8),allocatable              :: shell(:)
      ! Build all shells
      call xy_shells(lattice,shell_table,shell)
      ! Select the first
      nn_mask = abs(shell_table - shell(1)) < 1d-12
   end subroutine

   pure subroutine xy_next_nearest_neighbors(lattice,nnn_mask,nn_mask)
      !! Build a mask for the next-nearest neighbors of a given lattice.
      !! It is a NxN matrix, with N the number of sites in the lattice,
      !! storing .true. values only for the pairs of sites linked by a
      !! "NNN-bond". Optionally builds a nearest-neighbors mask, too.
      !! It calls internally xy_shells, for all inter-site
      !! distances, so if you need more than NNNs consider
      !! calling xy_shells directly. Note that we provide
      !! a similar subroutine for nearest-neighbors only,
      !! so if you just need NNs consider calling it.
      !! > call xy_nearest_neighbors(lattices,nn_maks)
      type(xy_lattice),intent(in)               :: lattice
      logical,allocatable,intent(out)           :: nnn_mask(:,:)
      logical,allocatable,intent(out),optional  :: nn_mask(:,:)
      real(8),allocatable                       :: shell_table(:,:)
      real(8),allocatable                       :: shell(:)
      ! Build all shells
      call xy_shells(lattice,shell_table,shell)
      if(present(nn_mask))then
         ! Select the NNs
         nn_mask = abs(shell_table - shell(1)) < 1d-12
      endif
      ! Select the NNNs
      nnn_mask = abs(shell_table - shell(2)) < 1d-12
   end subroutine

   pure subroutine xy_shells(lattice,shell_table,distance_set)
      !! Build a /ordered/ set of all inter-atomic distances
      !! in a given xy_lattice and a table storing distances
      !! among all sites. Searching this matrix for the n-th
      !! set entry gives a mask of all pairs of atomic sites
      !! that are n-th order neigbors.
      !! E.g. shell_table == distance_set(1) would provide a
      !! mask for nearest neighbors, from which we can build
      !! the tight-binding hopping term of the corresponding
      !! lattice hamiltonian.
      type(xy_lattice),intent(in)      :: lattice
      real(8),allocatable,intent(out)  :: distance_set(:)
      real(8),allocatable,intent(out)  :: shell_table(:,:)
      real(8)                          :: distance
      type(xy_site)                    :: A,B
      integer                          :: L,i,j,k
      L = size(lattice%site)
      allocate(shell_table(L,L),distance_set(L**2))
      shell_table = 0d0 ! init the shells
      distance_set = 0d0 ! and the set
      k = 0 ! unique distance counter
      do i = 1,L-1
         do j = i+1,L
            A = lattice%site(i)
            B = lattice%site(j)
            distance = xy_distance(A,B)
            shell_table(i,j) = distance
            shell_table(j,i) = distance
            if(all(abs(distance_set-distance)>1d-12))then
               k = k + 1
               distance_set(k) = distance
            end if
         enddo
      enddo
      ! Shrink the set to the actual required size:
      distance_set = pack(distance_set,distance_set/=0d0)
      call sort(distance_set)
   end subroutine xy_shells

   ! THESE ARE PRIVATE NAMES

   pure function ith_A_offset(layout,i) result(offset)
      !! Compute the offset vector connecting a site with label "A",
      !! to its i-th neighbor, returning a (scalar) xy coordinate.
      !! It takes any i ∈ ℤ, but there will only be 3 inequivalent
      !! output vectors, pointing to the three nearest neighbors.
      type(unit_cell),intent(in) :: layout
      integer,intent(in)         :: i
      type(xy)                   :: offset
      real(8)                    :: angle
      angle = 4d0*PI/N*(layout%orientation%angle+i) ! mixed math ok
      angle = angle + 2d0*pi/N * (1 - layout%orientation%angle)
      !-----> 60°,180°,300°...
      offset = xy(x=layout%size*cos(angle), y=layout%size*sin(angle))
   end function

   pure function ith_B_offset(layout,i) result(offset)
      !! Compute the offset vector connecting a site with label "B",
      !! to its i-th neighbor, returning a (scalar) xy coordinate.
      !! It takes any i ∈ ℤ, but there will only be 3 inequivalent
      !! output vectors, pointing to the three nearest neighbors.
      type(unit_cell),intent(in) :: layout
      integer,intent(in)         :: i
      type(xy)                   :: offset
      real(8)                    :: angle
      angle = 4d0*PI/N*(layout%orientation%angle+i) ! mixed math ok
      angle = angle + 2d0*pi/N * (2 - layout%orientation%angle)
      !-----> 120°,240°,360°...
      offset = xy(x=layout%size*cos(angle), y=layout%size*sin(angle))
   end function

end module xy_neighbors
