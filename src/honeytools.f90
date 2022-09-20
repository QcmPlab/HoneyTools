module honeytools
   !! Top level API: what you probably want to use

   use hex_coordinates
   use hex_layout
   use hex_neighbors
   use hex_geometries
   use xy_coordinates
   use xy_neighbors
   use honeyplots

   implicit none
   private

   public :: xy_lattice, xy_print, xy_shells
   public :: xy_nearest_neighbors, xy_next_nearest_neighbors
   public :: unit_cell, armchair, zigzag ! to generate layouts
   public :: get_supercell, get_triangle, get_flake, get_stripe, get_sublattice

contains

   pure function get_supercell(rows,cols,layout) &
      result(lattice)
      !! Get the lattice for a honeycomb supercell
      integer,intent(in)                  :: rows,cols
      type(unit_cell),intent(in)          :: layout
      type(xy_lattice)                    :: lattice
      type(hex),dimension(rows*cols)      :: hexagons
      hexagons = hex_supercell(rows,cols)
      lattice = hex2lattice(layout,hexagons)
   end function

   pure function get_triangle(size,layout) &
      result(lattice)
      !! Get the lattice for a triangle-shaped flake
      integer,intent(in)         :: size
      type(unit_cell),intent(in) :: layout
      type(xy_lattice)           :: lattice
      type(hex),allocatable      :: hexagons(:)
      hexagons = hex_triangle(size)
      lattice = hex2lattice(layout,hexagons)
   end function

   pure function get_flake(radius,layout) &
      result(lattice)
      !! Get the lattice for a hexagon-shaped flake
      integer,intent(in)         :: radius
      type(unit_cell),intent(in) :: layout
      type(xy_lattice)           :: lattice
      type(hex),allocatable      :: hexagons(:)
      hexagons = hex_flake(radius)
      lattice = hex2lattice(layout,hexagons)
   end function

   pure function get_stripe(height,width,layout) &
      result(lattice)
      !! Get the lattice for a hexagon-shaped flake
      integer,intent(in)         :: height,width
      type(unit_cell),intent(in) :: layout
      type(xy_lattice)           :: lattice
      type(hex),allocatable      :: hexagons(:)
      associate(cell => layout%orientation)
         if(cell==armchair)then
            hexagons = hex_armchair_stripe(height,width)
         elseif(cell==zigzag)then
            hexagons = hex_zigzag_stripe(height,width)
         else
         endif
         lattice = hex2lattice(layout,hexagons)
      end associate
   end function

end module honeytools
