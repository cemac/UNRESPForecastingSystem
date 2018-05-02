      Program readctg 

      parameter(in=10)
      parameter(nx=464,ny=400,mxcat=38,mxhit=40)
      parameter(x0=-335.0,y0=-258.0,dxy=1.0)
      character*120 fin

      dimension ngrid(mxcat,nx,ny)

      fin='fort.41'

      open(in,file=fin,status='old',action='read')

      ip=0      
      ngrid=0

 1000 read(in,*,end=2000)x,y,ilul,land,i,j,nn
      ngrid(land,i,j)=nn

      ip=ip+1

      goto 1000

 2000 print *,' ip=',ip

      do j=1,ny
         do i=1,nx
            do k=1,mxcat
               ilu=ngrid(k,i,j)
               if(ilu.ne.0) then
                  write(18,181)i,j,k,ilu
 181              format(4i6)
               endif
            enddo
         enddo
      enddo

      stop
      end
