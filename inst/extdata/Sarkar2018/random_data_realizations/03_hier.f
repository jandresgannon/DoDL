c---------Code to get heirarchy of a network---------
	integer ndim

	parameter(ndim=513)
	integer dist(ndim),IY(ndim)
	real*8 c(ndim),hier

	open(10,file='8_shrt_path.dat')
	open(50,file='8_hier_rand.dat')

	do i=1,ndim-1
          IY(i)=i
	  c(i)=0.d0
	  do j= i+1,ndim
	    dist(j)=0.d0
	    read(10,*)ii,jj,dist(j)
	    c(i) = c(i) + 1.d0/dfloat(dist(j))
	  enddo
	  c(i) = c(i)/dfloat(ndim-1)
	enddo
        IY(ndim)=ndim
c ----------- measuring the hierarchy---------

    
    	CALL DSORT(c,IY,ndim)

	hier=0.d0
	do i=1,ndim-1
	  write(60,*)i,c(i)
	  hier = hier + (c(1) - c(i))	
	enddo
	hier = hier/dfloat(ndim-1)
	write(50,*)hier

	END
