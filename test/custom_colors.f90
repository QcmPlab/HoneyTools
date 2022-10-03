program color_test

   use pyplot_module, only : pyplot, wp => pyplot_wp

   implicit none
   
   type(pyplot) :: plt   !! pytplot handler
   integer :: istat
   real(wp), parameter :: F(3) = [0.4510d0, 0.3098d0, 0.5882d0] ! Fortran-lang color 
   real(wp), parameter :: Y(3) = [0.9608d0, 0.8157d0, 0.0118d0] ! Yellow

   real(wp),dimension(:),allocatable :: Ax 
   real(wp),dimension(:),allocatable :: Ay 
   real(wp),dimension(:),allocatable :: Bx 
   real(wp),dimension(:),allocatable :: By 

   allocate(Ax(5),Ay(5))
   allocate(Bx(7),By(7))

   Ax = 0d0 
   Ay = 1d0
   Bx = 1d0
   By = 0d0

   call plt%initialize(xlabel='x',ylabel='y',axis_equal=.true.)

   call plt%add_plot(Ax,Ay,label='',linestyle='o',markersize=5,color=F)
   call plt%add_plot(Bx,By,label='',linestyle='o',markersize=5,color=Y)

   call plt%savefig('color_test.png', pyfile='color_test.py',istat=istat)

   end program color_test