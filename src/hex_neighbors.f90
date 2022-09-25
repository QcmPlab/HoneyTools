module hex_neighbors
   !! Defining nearest neighbors for given hex coordinates

   use hex_coordinates

   implicit none
   private

   public hex_direction, hex_hop, hex_nearest

   integer, parameter :: N = 6  ! Number of allowed directions

contains

   pure function hex_direction(idir) result(dir)
      !! Return a direction in pseudo-3D hex space, given a (signed) integer label
      integer,intent(in)      :: idir
      type(hex)               :: dir
      type(hex),dimension(N)  :: all_directions ! List of allowed directions
      all_directions = [hex( 1, 0,-1 ), &
                        hex( 1,-1, 0 ), &
                        hex( 0,-1, 1 ), &
                        hex(-1, 0, 1 ), &
                        hex(-1, 1, 0 ), &
                        hex( 0, 1,-1 )]
      dir = all_directions(1 + modulo(N + modulo(idir, N), N))
      ! this allows cycling: any signed integer is mapped to a valid i \in [1,6]
   end function

   pure function hex_hop(H,i) result(Ni)
      !! Return the nearest neighbor of H, 
      !! by hopping in the i-th direction.
      !! You can feed any i ∈ ℤ, but you'd
      !! get only 6 inequivalent neighbors
      type(hex),intent(in)    :: H  
      integer,intent(in)      :: i
      type(hex)               :: Ni
      Ni = H + hex_direction(i)
   end function

   pure function hex_nearest(H) result(nnvec)
      !! Return the vector of all nearest hex_neighbors of hex H
      type(hex),intent(in)    :: H
      type(hex),dimension(N)  :: nnvec
      integer                 :: i
      do i = 1,N
         nnvec(i) = hex_hop(H,i)
      enddo
   end function

end module hex_neighbors
