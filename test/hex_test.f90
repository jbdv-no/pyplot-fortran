!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/14/2015
!  license: BSD
!
!  udated: jbdv-no, date: 2023/02/26
!
!  Unit test for [[add_plot]] with [[hex_data]].

program test

   use pyplot_module, only: pyplot, wp => pyplot_wp

   implicit none

   integer, parameter :: nmin = 7
   integer, parameter :: nmax = 15
   real(wp), parameter :: pi = acos(-1.0_wp)
   real(wp), parameter :: deg2rad = pi/180.0_wp
   logical, parameter :: hexfmt(*) = [.false., .true.]

   character(1)  :: hextest
   integer       :: i      !! counter
   integer       :: j      !! counter
   integer       :: n      !! counter
   integer       :: istat  !! status code
   real(wp)      :: tic    !! time start value
   type(pyplot)  :: plt    !! pytplot handler

   do i = nmin, nmax

      block
         real(wp), dimension(:), allocatable :: x     !! x values
         real(wp), dimension(:), allocatable :: y     !! y values
         real(wp), dimension(:), allocatable :: sx    !! sin(x) values
         real(wp), dimension(:), allocatable :: cx    !! cos(x) values
         real(wp), dimension(:), allocatable :: tx    !! sin(x)*cos(x) values

         n = 2**i

         ! size arrays:
         allocate (x(n))
         allocate (y(n))
         allocate (sx(n))
         allocate (cx(n))
         allocate (tx(n))

         !generate some data:
         x = [(pi * (real(i, wp) / (n-1) - 1), i=0, size(x) - 1)]
         sx = sin(x)
         cx = cos(x)
         tx = sx*cx

         write (*, '("n = ", i0)') n

         do j = 1, size(hexfmt)

            write (hextest, '(g0)') hexfmt(j)

            !2d line plot:
            tic = get_walltime()
            call plt%initialize(grid=.true., xlabel='angle (rad)', figsize=[9, 6], &
                                title='plot test', legend=.true., axis_equal=.true., &
                                tight_layout=.true., &
                                hex_data=hexfmt(j))
            call plt%add_plot(x, sx, label='$\sin (x)$', linestyle='b-o', markersize=5, linewidth=2, istat=istat)
            call plt%add_plot(x, cx, label='$\cos (x)$', linestyle='r-o', markersize=5, linewidth=2, istat=istat)
            call plt%add_plot(x, tx, label='$\sin (x) \cos (x)$', linestyle='g-o', markersize=2, linewidth=1, &
                              ylim = [-1.5_wp, 1.5_wp], istat = istat)
            call plt%savefig('hextest_'//hextest//'.png', pyfile='hextest_'//hextest//'.py', istat=istat)
            write (*, '("  hex_data? ", a, ", duration = ", g0, "sec.")') hextest, get_walltime() - tic

         end do

      end block

   end do

contains

   function get_walltime() result(time)
        !! a very simple _wall clock_ routine for timing (relatively) short
        !! program sequences, based on: https://stackoverflow.com/a/53979065
      use, intrinsic :: iso_fortran_env, only: i8 => int64, r8 => real64
      real(r8)    :: time
      integer(i8) :: counter
      integer(i8) :: rate
      call system_clock(counter, rate)
      time = real(counter, kind=r8)/rate
   end function get_walltime

end program test
!*****************************************************************************************
