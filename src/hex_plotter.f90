module hex_plotter
   !! Providing plotting facilities for hex tessellations

   use ogpf
   use hex_coordinates
   use hex_layout
   use hex_neighbors
   use xy_coordinates

   implicit none
   private

   public :: hex_plot

   integer, parameter :: N = 6 ! Number of vertices in a hexagon

contains

   subroutine hex_plot(layout,hexagons)
      type(unit_cell),intent(in)       :: layout
      type(hex),intent(in)             :: hexagons(:)
      integer                          :: M
      type(gpf)                        :: plotter
      type(xy_tile),allocatable        :: corner(:)
      real(8),dimension(N)             :: xtmp,ytmp
      real(8),allocatable              :: x(:),y(:)
      integer                          :: i,j
      M = size(hexagons)
      allocate(x(N*M),y(N*M),corner(M))
      corner = hex2corner(layout,hexagons)
      do i = 1,M
         do j = 1,N
            xtmp(j) = corner(i)%vertex(j)%x
            ytmp(j) = corner(i)%vertex(j)%y
         enddo
         x((1+N*(i-1)):N*i) = xtmp
         y((1+N*(i-1)):N*i) = ytmp
      enddo
      call plotter%xlabel('x')
      call plotter%ylabel('y')
      call plotter%filename("hex_temp_script.gp")
      call plotter%plot(x,y,'with points pt 6 ps 1.2 lc rgb "#000000"')
      deallocate(x,y)
   end subroutine


end module
