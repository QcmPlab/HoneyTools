module honeyplots
   !! Providing plotting facilities for hex tessellations

   use pyplot_module
   use ogpf
   use hex_coordinates
   use hex_layout
   use hex_neighbors
   use xy_coordinates
   use xy_neighbors

   implicit none
   private

   public :: xy_plot, hex_plot, plot

   integer, parameter :: N = 6 ! Number of vertices in a hexagon

   interface plot
      procedure :: xy_plot
      procedure :: hex_plot
   end interface

contains

   subroutine xy_plot(lattice,nn_mask,nnn_mask,backend,figure_name,script_name,set_terminal)
      !! >>> CURRENTLY GNUPLOT DOES NOT SUPPORT NN AND NNN LINKS, HELP IS WELCOME!
      !! Simple plotter for class(xy_lattice) objects. If optional argument nn_mask
      !! is passed it draws the nearest-neighbor bonds too. Similarly, if optional
      !! argument nnn_mask is passed a dashed link to next-nearest neighbors would
      !! be drawed. Otherwise only lallice sites. It features two backends, giving
      !! access to either matplotlib(pyplot) or gnuplot internal machinery. Either
      !! library needs to be installed in your system for hex_plot to work.
      !! You can specify a figure_name to save the plot to disk. If not specified
      !! the plot would be just displayed in a popup window (or the terminal with
      !! suitable configuration of the gnuplot backend) and a corresponding script
      !! saved, aiming at fast reproduction (the script will all contain data).
      !! The script has a default name (hex_plot.{py,gp}), but you can optionally
      !! give your own custom string, do not forget the appropriate file extension.
      !! Regarding output formats instead, the two backends have slightly different
      !! behavior: pyplot would infer the format from the given file extension in
      !! figure_name, whilst gnuplot does need a proper "output terminal" to be set.
      !! You can do that by passing the optional set_parameter variable. Refer to
      !! original gnuplot documentation for the list of available terminals and how
      !! they should be chosen. For convenience we report that you can get the list
      !! of (system dependent) available terminals in your console by executing the
      !! gnuplot -e "set terminal" command in your shell. Furthermore all systems
      !! should be compatible with the "dumb" terminal option, which would direct
      !! the plot to the terminal itself, in the form of ASCII art. This can be
      !! very useful for HPC workflows.
      type(xy_lattice),intent(in)             :: lattice
      logical,allocatable,intent(in),optional :: nn_mask(:,:)
      logical,allocatable,intent(in),optional :: nnn_mask(:,:)
      character(*),intent(in),optional        :: backend !! default: "pyplot" (or "gnuplot")
      character(*),intent(in),optional        :: figure_name
      character(*),intent(in),optional        :: script_name
      character(*),intent(in),optional        :: set_terminal !! relevant to gnuplot backend
      type(xy_lattice)                        :: sublattice
      character(8)                            :: engine
      character(32)                           :: source_name
      real(8),allocatable                     :: Ax(:),Ay(:)
      real(8),allocatable                     :: Bx(:),By(:)
      real(8),dimension(2)                    :: x,y,x1,y1,x2,y2
      type(pyplot)                            :: plt
      type(gpf)                               :: gnu
      integer                                 :: i,j,k,l

      sublattice = get_sublattice(lattice,"A")
      Ax = sublattice%site%x
      Ay = sublattice%site%y

      sublattice = get_sublattice(lattice,"B")
      Bx = sublattice%site%x
      By = sublattice%site%y

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
         ! Nearest Neighbor Links
         if(present(nn_mask))then
            do i = 1, size(nn_mask,1)
               do j = 1, size(nn_mask,1)
                  if(nn_mask(i,j))then
                     x(1) = lattice%site(i)%x
                     x(2) = lattice%site(j)%x
                     y(1) = lattice%site(i)%y
                     y(2) = lattice%site(j)%y
                     call plt%add_plot(x,y,label='',linestyle='-k',linewidth=2)
                  endif
               enddo
            enddo
         endif
         ! Next Nearest Neighbors
         if(present(nnn_mask))then
            do i = 1, size(nnn_mask,1)
               do j = 1, size(nnn_mask,1)
                  if(nnn_mask(i,j))then
                     x(1) = lattice%site(i)%x
                     x(2) = lattice%site(j)%x
                     y(1) = lattice%site(i)%y
                     y(2) = lattice%site(j)%y
                     call plt%add_plot(x,y,label='',linestyle=':k',linewidth=1)
                  endif
               enddo
            enddo
         endif
         ! Sublattice "A"
         call plt%add_plot(Ax,Ay,label='',linestyle='o',markersize=5)
         ! Sublattice "B
         call plt%add_plot(Bx,By,label='',linestyle='o',markersize=5)

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

         if(present(set_terminal))then
            call gnu%options("set term "//set_terminal//";")
         else
            call gnu%options("set term qt;")
         endif

         if(present(figure_name))then
            call gnu%options('set output "'//figure_name//'"')
         endif

         call gnu%options("set size ratio -1;") ! --> axis equal
         call gnu%options("unset grid") ! --> grid off
         call gnu%xlabel('x')
         call gnu%ylabel('y')
         call gnu%filename(source_name)
         print*
         print*, "> Gnuplot GUI popping up..."
         print*
         call gnu%plot(                                        &
            x1=Ax, y1=Ay,                                      &
            ls1='with points pt 7 ps 1 lc rgb "#1F77B4"',      &
            x2=Bx, y2=By,                                      &
            ls2='with points pt 7 ps 1 lc rgb "#FF7F0E"'       &
            )

      end select

   end subroutine xy_plot

   subroutine hex_plot(layout,hexagons,backend,figure_name,script_name,set_terminal)
      !! Simple plotter for arrays of type(hex). It features two backends, giving
      !! access to either matplotlib(pyplot) or gnuplot internal machinery. Either
      !! library needs to be installed in your system for hex_plot to work.
      !! You can specify a figure_name to save the plot to disk. If not specified
      !! the plot would be just displayed in a popup window (or the terminal with
      !! suitable configuration of the gnuplot backend) and a corresponding script
      !! saved, aiming at fast reproduction (the script will all contain data).
      !! The script has a default name (hex_plot.{py,gp}), but you can optionally
      !! give your own custom string, do not forget the appropriate file extension.
      !! Regarding output formats instead, the two backends have slightly different
      !! behavior: pyplot would infer the format from the given file extension in
      !! figure_name, whilst gnuplot does need a proper "output terminal" to be set.
      !! You can do that by passing the optional set_parameter variable. Refer to
      !! original gnuplot documentation for the list of available terminals and how
      !! they should be chosen. For convenience we report that you can get the list
      !! of (system dependent) available terminals in your console by executing the
      !! gnuplot -e "set terminal" command in your shell. Furthermore all systems
      !! should be compatible with the "dumb" terminal option, which would direct
      !! the plot to the terminal itself, in the form of ASCII art. This can be
      !! very useful for HPC workflows.
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
      type(xy_lattice),allocatable     :: corner(:)
      real(8),dimension(N)             :: xtmp,ytmp
      real(8),allocatable              :: x(:),y(:)
      integer                          :: i,j

      M = size(hexagons)
      allocate(x(N*M),y(N*M),corner(M))
      corner = hex2corner(layout,hexagons)

      do i = 1,M
         do j = 1,N
            xtmp(j) = corner(i)%site(j)%x
            ytmp(j) = corner(i)%site(j)%y
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
         call plt%add_plot(x,y,label='',linestyle='o',markersize=5)

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

         if(present(set_terminal))then
            call gnu%options("set term "//set_terminal//";")
         else
            call gnu%options("set term qt;")
         endif

         if(present(figure_name))then
            call gnu%options('set output "'//figure_name//'"')
         endif

         call gnu%options("set size ratio -1;") ! --> axis equal
         call gnu%options("unset grid") ! --> grid off
         call gnu%xlabel('x')
         call gnu%ylabel('y')
         call gnu%filename(source_name)
         print*
         print*, "> Gnuplot GUI popping up..."
         print*
         call gnu%plot(x,y,'with points pt 7 ps 1 lc rgb "#1F77B4"')

      end select

   end subroutine hex_plot

end module honeyplots
