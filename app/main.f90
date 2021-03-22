program main

    use, intrinsic :: iso_fortran_env, only : sp=>real32, dp=>real64, ip=>int32
    use h5fortran,  only: hdf5_file, HSIZE_T

    implicit none

    character(len=:), allocatable :: filename
    character(len=:), allocatable :: dataname
    real(dp), allocatable         :: be_position(:)
    real(dp), allocatable         :: be_chord(:)
    real(dp), allocatable         :: be_chord_le(:)
    real(dp), allocatable         :: be_chord_te(:)
    real(dp), allocatable         :: be_width(:)
    real(dp)                      :: length
    integer                       :: ierr
    integer                       :: i

    filename = '/home/wbd/work/programming/python/fly_aero_data/wing_data/fly_param.hdf5'

    dataname = '/wing/left/blade_element/position'
    call load_real_1d_dataset(filename, dataname,  be_position, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    dataname = '/wing/left/blade_element/chord'
    call load_real_1d_dataset(filename, dataname, be_chord, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    dataname = '/wing/left/blade_element/chord_leading'
    call load_real_1d_dataset(filename, dataname, be_chord_le, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    dataname = '/wing/left/blade_element/chord_trailing'
    call load_real_1d_dataset(filename, dataname, be_chord_te, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    dataname = '/wing/left/blade_element/width'
    call load_real_1d_dataset(filename, dataname, be_width, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    dataname = '/wing/left/length'
    call load_real_scalar_dataset(filename, dataname, length, ierr)
    if ( ierr /= 0) error stop ': unable to load dataset: '//dataname

    print *, 'wing length: ', length
    print *, 'size(be_position): ', size(be_position)

    !do i=1,size(be_position)
    !    print *, i, be_position(i), be_chord(i), be_chord_le(i), be_chord_te(i), be_width(i)
    !end do

contains

    subroutine load_real_1d_dataset(filename, dataname, dataset, ierr)
        character(*), intent(in)             :: filename
        character(*), intent(in)             :: dataname
        real(dp), intent(inout), allocatable :: dataset(:)
        integer, intent(inout)               :: ierr

        ! locad variables
        type(hdf5_file)               :: h5file
        logical                       :: exists
        integer                       :: ndims
        integer(HSIZE_T), allocatable :: dims(:)

        call h5file % initialize(filename, ierr, status='old', action='r') 
        if ( ierr /= 0 ) return 

        exists = h5file % exists(dataname)
        if (.not. exists) then
            ! Maybe set some type of error code?
            ierr = 1  
            return
        end if

        ndims = h5file % ndims(dataname)
        if (ndims /= 1) then
            ! Maybe set some type of error code?
            ierr = 1
            return
        end if
        
        call h5file % shape(dataname, dims, ierr)
        if ( ierr /= 0 ) return 

        if (allocated(dataset)) then
            deallocate(dataset)
        endif
        allocate(dataset(dims(1)), stat=ierr)
        if (ierr /= 0 ) return

        call h5file % read(dataname, dataset, ierr)
        if ( ierr /=0 ) return 

        call h5file % finalize(ierr)
        if (ierr /=0 ) return 
    end subroutine load_real_1d_dataset


    subroutine load_real_scalar_dataset(filename, dataname, dataset, ierr)
        character(*), intent(in)             :: filename
        character(*), intent(in)             :: dataname
        real(dp), intent(inout)              :: dataset
        integer, intent(inout)               :: ierr

        ! locad variables
        type(hdf5_file) :: h5file
        logical         :: exists
        integer         :: ndims

        call h5file % initialize(filename, ierr, status='old', action='r') 
        if ( ierr /= 0 ) return 

        exists = h5file % exists(dataname)
        if (.not. exists) then
            ! Maybe set some type of error code?
            ierr = 1  
            return
        end if

        ndims = h5file % ndims(dataname)
        if ( ndims /= 0 ) then 
            ierr = 1
            return 
        end if

        call h5file % read(dataname, dataset, ierr)
        if ( ierr /=0 ) return 

        call h5file % finalize(ierr)
        if (ierr /=0 ) return 
    end subroutine load_real_scalar_dataset


end program main
