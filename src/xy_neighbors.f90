module xy_neighbors
   !! Defining all neighbors shells for given xy lattices

   use xy_coordinates
   use stdlib_sorting, only: sort !=> ord_sort (impure)

   implicit none
   private

   public xy_shells, xy_nearest_neighbors, xy_next_nearest_neighbors

contains

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

end module xy_neighbors

