module hex_plotter
   !! Providing plotting facilities for hex tessellations

   use pyplot_module
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

   subroutine hex_plot(layout,hexagons,backend,figure_name,script_name,set_terminal)
      type(unit_cell),intent(in)       :: layout
      type(hex),intent(in)             :: hexagons(:)
      character(*),intent(in),optional :: backend !! default: "pyplot" (or "gnuplot")
      character(*),intent(in),optional :: figure_name
      character(*),intent(in),optional :: script_name
      character(*),intent(in),optional :: set_terminal !! relevant to gnuplot backend
      character(8)                     :: engine
      character(32)                    :: source_name
      integer                          :: M
      type(pyplot)                     :: plt
      type(gpf)                        :: gnu
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

      if(present(backend))then
         engine = trim(backend)
      else
         engine = "pyplot"
      end if

      select case(trim(engine))

       case default
         print*, "unknown backend: no plot generated"

       case ("pyplot")

         call plt%initialize(xlabel='x',ylabel='y',axis_equal=.true.)
         call plt%add_plot(x,y,label='',linestyle='o',markersize=15)

         if(present(script_name))then
            source_name = script_name
         else
            source_name = "hex_plot.py"
         endif

         if(present(figure_name))then
            call plt%savefig(trim(figure_name), pyfile=trim(source_name))
            print*
            print*, "> PyPlot figure saved to: "//trim(figure_name)
            print*
         else
            print*
            print*, "> PyPlot GUI popping up..."
            print*
            call plt%showfig(pyfile=trim(source_name))
         endif

       case ("gnuplot")

         if(present(script_name))then
            source_name = script_name
         else
            source_name = "hex_plot.gp"
         endif

         if(present(script_name))then
            source_name = script_name
         else
            source_name = "hex_plot.gp"
         endif

         if(present(set_terminal))then
            call gnu%options("set term "//set_terminal//";")
         else
            call gnu%options("set term qt;")
         endif
         
         if(present(figure_name))then
            call gnu%options('set output "'//figure_name//'"')
         endif

         call gnu%options("set size ratio -1;")
         call gnu%options("unset grid")
         call gnu%xlabel('x')
         call gnu%ylabel('y')
         call gnu%filename(source_name)
         print*
         print*, "> Gnuplot GUI popping up..."
         print*
         call gnu%plot(x,y,'with circles pt 6 lc rgb "#1F77B4" fill solid noborder')

      end select

      deallocate(x,y)

   end subroutine


end module
