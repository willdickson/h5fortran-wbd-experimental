submodule (h5fortran:read) reader_lt

    implicit none (type, external)

contains

    module procedure h5exist
        type(hdf5_file) :: h
        
        call h%initialize(filename, status='old', action='r')
        h5exist = h%exist(dname)
        call h%finalize()
    end procedure h5exist
    
    
    module procedure lt0read
        type(hdf5_file) :: h
        integer :: ier
        
        call h%initialize(filename, ier, status='old', action='r')
        if (ier == 0) call h%read(dname, value, ier)
        if (ier == 0) call h%finalize(ier)
        
        if (present(ierr)) ierr = ier
        if (check(ier, filename, dname) .and. .not.present(ierr)) error stop
    end procedure lt0read
    
    module procedure lt1read
        type(hdf5_file) :: h
        integer :: ier
        
        call h%initialize(filename, ier, status='old', action='r')
        if (ier == 0) call h%read(dname, value, ier)
        if (ier == 0) call h%finalize(ier)
        
        if (present(ierr)) ierr = ier
        if (check(ier, filename, dname) .and. .not.present(ierr)) error stop
    end procedure lt1read
    
    module procedure lt2read
        type(hdf5_file) :: h
        integer :: ier
        
        call h%initialize(filename, ier, status='old', action='r')
        if (ier == 0) call h%read(dname, value, ier)
        if (ier == 0) call h%finalize(ier)
        
        if (present(ierr)) ierr = ier
        if (check(ier, filename, dname) .and. .not.present(ierr)) error stop
    end procedure lt2read
    
    module procedure lt3read
        type(hdf5_file) :: h
        integer :: ier
        
        call h%initialize(filename, ier, status='old', action='r')
        if (ier == 0) call h%read(dname, value, ier)
        if (ier == 0) call h%finalize(ier)
        
        if (present(ierr)) ierr = ier
        if (check(ier, filename, dname) .and. .not.present(ierr)) error stop
    end procedure lt3read
    
    module procedure lt4read
        type(hdf5_file) :: h
        integer :: ier
        
        call h%initialize(filename, ier, status='old', action='r')
        if (ier == 0) call h%read(dname, value, ier)
        if (ier == 0) call h%finalize(ier)
        
        if (present(ierr)) ierr = ier
        if (check(ier, filename, dname) .and. .not.present(ierr)) error stop
    end procedure lt4read
    
    module procedure lt5read
        type(hdf5_file) :: h
        integer :: ier
        
        call h%initialize(filename, ier, status='old', action='r')
        if (ier == 0) call h%read(dname, value, ier)
        if (ier == 0) call h%finalize(ier)
        
        if (present(ierr)) ierr = ier
        if (check(ier, filename, dname) .and. .not.present(ierr)) error stop
    end procedure lt5read
    
    module procedure lt6read
        type(hdf5_file) :: h
        integer :: ier
        
        call h%initialize(filename, ier, status='old', action='r')
        if (ier == 0) call h%read(dname, value, ier)
        if (ier == 0) call h%finalize(ier)
        
        if (present(ierr)) ierr = ier
        if (check(ier, filename, dname) .and. .not.present(ierr)) error stop
    end procedure lt6read
    
    module procedure lt7read
        type(hdf5_file) :: h
        integer :: ier
        
        call h%initialize(filename, ier, status='old', action='r')
        if (ier == 0) call h%read(dname, value, ier)
        if (ier == 0) call h%finalize(ier)
        
        if (present(ierr)) ierr = ier
        if (check(ier, filename, dname) .and. .not.present(ierr)) error stop
    end procedure lt7read
    
end submodule reader_lt
