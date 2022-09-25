module xy_coordinates
   !! Defining real space 2D coordinates for honeycomb lattice sites

   use hex_coordinates
   use hex_layout
   use assert_m, only : assert ![pure assertion]

   implicit none
   private

   public :: xy, xy_site, xy_lattice                        ! xy class hyerarchy
   public :: xy_distance, xy_print, xy_ordered_union        ! xy-bound procedures
   public :: hex2center, hex2corner, hex2site, hex2lattice  ! hex-2-xy procedures
   public :: get_sublattice                                 ! xy_lattice utility

   integer, parameter :: N = 6 ! Number of vertices in a hexagon
   real(8), parameter :: PI = 4d0*atan(1d0) ! π to selected kind

   type xy
      !! Base type for 2D points
      real(8) :: x
      real(8) :: y
   contains
      generic :: operator(==) => eq_xy
      generic :: operator(/=) => neq_xy
      procedure, private :: eq_xy
      procedure, private :: neq_xy
   end type

   type, extends(xy) :: xy_site
      !! A 2D point extension for inequivalent sites
      character(1) :: label !! A or B (sublattice)
      integer      :: key=0 !! for lattice lookup
   end type

   type xy_lattice
      !! Wrapper type for storing dynamically
      !! sized collections of lattice sites
      type(xy_site),allocatable  :: site(:)
      integer                    :: size
   contains
      generic :: operator(==) => eq_lattice
      generic :: operator(/=) => neq_lattice
      procedure, private :: eq_lattice
      procedure, private :: neq_lattice
      procedure :: push_back
   end type

   interface xy_print
      procedure :: xy_class_print
      procedure :: xy_lattice_print
   end interface

contains

   ! PUBLIC API [private at bottom]

   pure elemental function xy_distance(A,B) result(d)
      !! Polymorphic euclidean distance for xy class
      class(xy),intent(in)    :: A,B
      type(xy)                :: C
      real(8)                 :: d
      C % x = A % x - B % x
      C % y = A % y - B % y
      d = xy_norm(C)
   end function

   pure elemental function hex2site(layout,H,label) result(site)
      !! Convert hex coordinates to real 2D lattice sites
      !! [returning the xy coordinates for the unique unit-cell,
      !!  sites "A" and "B": this of course does not account for
      !!  border effects, so would be suitable only for simple,
      !!  periodized, systems, which for now are out of scope.]
      !! Actual real-space layout has to be specified by passing
      !! a (scalar) unit_cell object.
      type(unit_cell),intent(in) :: layout
      type(hex),intent(in)       :: H
      character(1),intent(in)    :: label  !! A or B
      type(xy)                   :: center
      type(xy)                   :: offset
      type(xy_site)              :: site
      center = hex2center(layout,H)
      select case(label)
       case("A")
         offset = ith_corner_offset(layout,i=1)
       case("B")
         offset = ith_corner_offset(layout,i=2)
       case default
         error stop "Honeycomb sites must have 'A' or 'B' label"
      end select
      site%x = center%x + offset%x
      site%y = center%y + offset%y
   end function

   pure elemental function hex2corner(layout,H) result(corner)
      !! Convert hex coordinates to real 2D space
      !! [returning the xy coordinates for the hexagon corners,
      !!  as appropriatiely wrapped in the "xy_lattice" type]
      !! Actual real-space layout has to be specified by passing
      !! a (scalar) unit_cell object.
      type(unit_cell),intent(in) :: layout
      type(hex),intent(in)       :: H
      type(xy)                   :: center
      type(xy)                   :: offset
      type(xy_lattice)           :: corner
      integer                    :: i
      center = hex2center(layout,H)
      allocate(corner%site(N))
      corner%size = N
      do i = 1,N
         offset = ith_corner_offset(layout,i)
         corner%site(i)%x = center%x + offset%x
         corner%site(i)%y = center%y + offset%y
         if(modulo(i,2)==1)then
            corner%site(i)%label = "A"
         else
            corner%site(i)%label = "B"
         endif
      enddo
   end function

   pure elemental function hex2center(layout,H) result(center)
      !! Convert hex coordinates to real 2D space
      !! [returning the xy coordinates of the hexagon center]
      !! Actual real-space layout has to be specified by passing
      !! a (scalar) unit_cell object.
      type(unit_cell),intent(in) :: layout
      type(hex),intent(in)       :: H
      type(xy)                   :: center
      type(hex_orientation)      :: basis
      ! Project [q,r] along unit-cell basis to get [x,y]
      basis = layout%orientation
      center%x = basis%uq(1) * H%q + basis%ur(1) * H%r ! mixed math ok
      center%y = basis%uq(2) * H%q + basis%ur(2) * H%r ! mixed math ok
      ! Rescale and recenter according to unit-cell data
      center%x = center%x * layout%size + layout%origin(1)
      center%y = center%y * layout%size + layout%origin(2)
      ! Equivalent to:
      !        ⎡x⎤   ⎡uq(1)  ur(1)⎤   ⎡q⎤
      !        ⎥ ⎥ = ⎥            ⎥ × ⎥ ⎥ × size + origin
      !        ⎣y⎦   ⎣uq(2)  ur(2)⎦   ⎣r⎦
   end function

   pure function hex2lattice(layout,hexagons) result(lattice)
      !! Generate a type(xy_lattice) object from any given hex
      !! array, provided a suitable layout (unit-cell)
      type(unit_cell),intent(in) :: layout
      type(hex),intent(in)       :: hexagons(:)
      type(xy_lattice)           :: corners(size(hexagons))
      type(xy_lattice)           :: lattice
      integer                    :: i
      corners = hex2corner(layout,hexagons)
      if(size(corners)==1)then
         lattice = corners(1)
         return
      endif
      lattice = xy_ordered_union(corners(1),corners(2))
      do i = 3,size(hexagons)
         lattice = xy_ordered_union(lattice,corners(i))
      enddo
   end function

   pure elemental function get_sublattice(lattice,label) result(sublattice)
      !! Extract sublattice, given a lattice and a label ("A" or "B")
      type(xy_lattice),intent(in)   :: lattice
      character(1),intent(in)       :: label  !! A or B
      type(xy_lattice)              :: sublattice
      associate (sites => lattice%site)
         sublattice%site = pack(sites,sites%label==label)
      end associate
   end function

   pure function xy_ordered_union(A,B) result(C)
      !! An ordered-keys set union for xy_lattices.
      !! It compares the lattice sites, looking at
      !! their x and y coordinates only: equal x,y
      !! entries are inserted in C just once, so to
      !! make it a set. A and B must be sets too.
      !! The sublattice labels are preserved in the
      !! process, assuming that two sites with the
      !! same x,y pertain to the same sublattice.
      !! For A the keys are assumed to be 1:size(A),
      !! and preserved as such (with an assertion).
      !! The keys of B are instead discarded, so to
      !! be replaced by size(A):size(C), which thus
      !! amounts to have result C uniquely indexed
      !! as 1, 2, ... , size(A) + size(B).
      !! This allows building consistently indexed
      !! matrices to act on the lattice array, such
      !! as real-space tight-binding hamiltonians.
      !! The keys would then be used to index other
      !! real space quantities, such as LDOS, Chern
      !! marker, local magnetization, and so on.
      type(xy_lattice),intent(in)   :: A
      type(xy_lattice),intent(in)   :: B
      type(xy_lattice)              :: C
      integer                       :: i,j
      C = A !copy all data of A into C
      do j = 1,size(A%site)
         if(C%site(j)%key /= j)then
            C%site(j)%key  = j
         endif
      enddo
      do i = 1,size(B%site)
         if(all(B%site(i) /= A%site))then
            call C%push_back(B%site(i))
            j = size(C%site)
            C%site(j)%key=j
         endif
      enddo
      ! What if we allocate to (size(A)+size(B))
      ! memory to C, fill in the unique elements
      ! and then just truncate to the correct size
      ! by just calling pack(C,C/=0)? Assuming of
      ! course that C has been initialized to 0d0.
      ! Would it perform better than dynamically
      ! growing the array at each insertion? 0ˆ0-,
   end function

   ! THESE ARE PRIVATE NAMES

   pure function ith_corner_offset(layout,i) result(offset)
      !! Compute the i-th offset vector connecting the hexagon center
      !! to its corners, returning a (scalar) xy coordinate
      type(unit_cell),intent(in) :: layout
      integer,intent(in)         :: i
      type(xy)                   :: offset
      real(8)                    :: angle
      angle = 2d0*PI/N * (layout%orientation%angle + i) ! mixed math ok
      offset = xy(x=layout%size*cos(angle), y=layout%size*sin(angle))
   end function

   pure elemental function eq_xy(A,B) result(isequal)
      !! tolerance equality for xy class
      !! [absolute tol hard-coded to 1d-12]
      !! -> probably to improve...
      class(xy),intent(in) :: A,B
      logical              :: isequal
      isequal = abs(A%x - B%x) < 1d-12
      isequal = abs(A%y - B%y) < 1d-12 .and. isequal
   end function

   pure elemental function neq_xy(A,B) result(notequal)
      !! tolerance inequality for xy class
      !! [absolute tol hard-coded to 1d-12]
      !! -> probably to improve...
      class(xy),intent(in) :: A,B
      logical              :: notequal
      notequal = .not.(eq_xy(A,B))
   end function

   pure elemental function eq_lattice(A,B) result(isequal)
      !! polymorphic equality overload for xy_lattice type
      class(xy_lattice),intent(in)  :: A,B
      logical                       :: isequal
      isequal = all(eq_xy(A%site,B%site))
   end function

   pure elemental function neq_lattice(A,B) result(notequal)
      !! polymorphic inequality overload for xy_lattice type
      class(xy_lattice),intent(in)  :: A,B
      logical                       :: notequal
      notequal = .not.(eq_lattice(A,B))
   end function

   pure elemental function xy_norm(A) result(r)
      !! polymorphic euclidean norm for xy class
      class(xy),intent(in) :: A
      real(8)              :: r
      r = sqrt(A%x**2 + A%y**2)
   end function

   pure subroutine push_back(vec,val)
      !! Poor man implementation of a dynamic
      !! array, a là std::vector (but without
      !! preallocation and smart doubling...)
      class(xy_lattice),intent(inout)  :: vec
      type(xy_site),intent(in)         :: val
      type(xy_site),allocatable        :: tmp(:)
      integer                          :: len
      if (allocated(vec%site)) then
         len = size(vec%site)
         allocate(tmp(len+1))
         tmp(:len) = vec%site
         call move_alloc(tmp,vec%site)
         len = len + 1
      else
         len = 1
         allocate(vec%site(len))
      end if
      !PUSH val at the BACK
      vec%site(len) = val
      !Increade formal size
      vec%size = len
   end subroutine push_back

   impure elemental subroutine xy_class_print(S,unit,quiet)
      !! Pretty print of xy coordinates in static arrays
      class(xy),intent(in)         :: S
      integer,intent(in),optional  :: unit  !! default = $stdout
      logical,intent(in),optional  :: quiet !! default = .false.
      integer                      :: stdunit
      logical                      :: verbose
      if(present(quiet))then
         verbose = .not.quiet
      else
         verbose = .true.
      endif
      if(present(unit))then
         stdunit = unit
      else
         stdunit = 6 ! stdout
      endif
      if(verbose)then
         select type(S)
          type is(xy_site)
            write(stdunit,*) "site #", S%key, "[x,y]: ", S%x, S%y
          class default
            write(stdunit,*) "real-space coordinates [x,y]: ", S%x, S%y
         end select
      else
         write(stdunit,*) S%x, S%y
      endif
   end subroutine

   impure elemental subroutine xy_lattice_print(S,unit,quiet)
      !! Pretty print of xy coordinates in dynamic arrays
      type(xy_lattice),intent(in)  :: S
      integer,intent(in),optional  :: unit  !! default = $stdout
      logical,intent(in),optional  :: quiet !! default = .false.
      type(xy_site)                :: vector(size(S%site))
      integer                      :: stdunit
      logical                      :: verbose
      integer                      :: i
      if(present(quiet))then
         verbose = .not.quiet
      else
         verbose = .true.
      endif
      if(present(unit))then
         stdunit = unit
      else
         stdunit = 6 ! stdout
      endif
      vector = S%site
      do i = 1,size(vector)
         call xy_print(vector(i),unit=stdunit,quiet=.not.verbose)
      enddo
   end subroutine

end module xy_coordinates
