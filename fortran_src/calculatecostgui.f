
      subroutine calculatecostgui
     & (costmethod,movetype,bothways,usf,osf,corridorwidth,
     &       dem,nullvalue,xsize,ysize,coords,maxtime,
     &       numdestinations,dx,dy,cost,time,pathx,pathy,
     &        errcode)

C 
C This version created to separate arguments costmethod (Tobler etc), movetype (queen's etc), usf (initial undersample factor)
C and osf (final oversample factor) and pass in bothways logical argument. Before these were squeezed into two arguments. 

C  On input of array of elevations, a start point and a number of destinations,
C  compute a field of times (cost) calculated on the way to those destinations.
C  Uses Dijkstra's algorithm, treating each pixel as a network node computing the edge values
C  as time taken between adjacent pixels using Tobler's hiking function or similar, with option to
C minimise energy rather than time. Cost is the array
C  of minimum times or energy to get to each pixel. Also calculates paths by following the backlink
C matrix.
C If 'fine' option selected, creates a high resolution sub-grid around each 1 km section
C of the path, rotated so that its last point is positioned due east of its first.
C This both allows turn angles smaller than queen's move 45 degrees -  e.g. with factor set
C to 5 gives around 9 degree turn resolution - and minimises the number of turns necessary.
C Experiments with 105 paths show this to shorten paths to about 96.3% of standard, c.f. queen's move
C theoretical 95.0%. NB factor used to be taken from figure after decimal on usffac.

C NB this version can determine a 'both ways' route, i.e. one which minimises time for a return journey
C to reflect the fact that only one route is generally taken for a road to serve both directions. To
C indicate this, add 5 to costmethod.

       implicit none


       integer, parameter :: dp = selected_real_kind(15, 307)
c this allows the (kind=dp) declarations below.




       integer movetype,costmethod,xsize,ysize,numdestinations,
     &         coords(0:numdestinations,0:1),errcode,unreachable,!fac,
     &         oldcoords(0:numdestinations,0:1),usf,osf

C movetype 1 for queen, 2 for queen + knight in the units.
C 10s indicate method of cost calculation: 0 for modified tobler,
C 1 for Tobler, 2 for Naismith and 3 for Minetti energy. 4 indicates sea travel.
C e.g. 1 is modified Tobler with queen's move, 32 is Minetti
C with knight's move, 41 queen's move on ship.

       real(kind=dp) dem(0:xsize-1,0:ysize-1),corridorwidth,
     &        cost(0:xsize-1,0:ysize-1), nullvalue,dx,dy,maxtime


       logical visited   (-10:xsize+9,-10:ysize+9),!this extra space to take account of scaling of jumps
     &         incorridor(-10:xsize+9,-10:ysize+9), !this extra space to take account of scaling of jumps
c larger array to create boundary of 'visited' pixels to prevent straying onto edge
     &         reached(numdestinations),
     &         repositioned(0:numdestinations),
     &         bothways

       integer destinationsreached,i,j,newx,newy,x,y,
     &        costdir(0:xsize-1,0:ysize-1)

       integer
     &       tbnvx(3*(xsize+ysize)*movetype),
     &       tbnvy(3*(xsize+ysize)*movetype),tbnvcount,
     &       mink,k,movex(16),movey(16),nummoves,minj


c tbnv stands for 'touched but not visited'

       real(kind=dp) dh,ds,mincost,
     & time(0:numdestinations*3),costk,tempcost,bignumber,dist,temptime,
     & distance(16),x1,x2,y1,y2,alpha,beta,tempdist,temptimereturn,
     & critslopeup,critslopedown

      integer count,zag,
     &        mini,deltax,deltay

      real(kind=dp)
C pathx and y used to be integers but now real to get sub grid scale zigzags.
     &        pathx(0:numdestinations,0:(xsize+ysize)*2),
     &        pathy(0:numdestinations,0:(xsize+ysize)*2),
     &        oldpathx(0:numdestinations,0:(xsize+ysize)*2),
     &        oldpathy(0:numdestinations,0:(xsize+ysize)*2),

     &  m,c,xmarg,ymarg,stld,realx,mindist, !was maxtime
     &              c2,ellipsey

C      logical quadreached(4)

      logical calculatefine, stilllooking

      real(kind=dp) gamma,
     &              maxx,minx,maxy,miny,length,rotatedx,rotatedy,
     &              startx,starty,endy,endx,rdx,rdy,
     &              ix,iy,subdistance(16),timeratio,distratio,dist2

      integer subcount,ii,numx,numy,no_interp_pts,lastx,lasty

      integer segstart,segend,seglength,kount

c the following are variables whose size depends on the size of the subgrid which in turn
C depends on the shape of the rotated path.

      real(kind=dp), dimension (:),  allocatable :: xcoord,ycoord
      real(kind=dp), dimension (:),  allocatable :: interpolated_values
      real(kind=dp), dimension (:,:), allocatable :: subdem,subcost
      logical,       dimension (:,:), allocatable :: subvisited
      integer,       dimension (:,:), allocatable :: subcostdir



      procedure (real(kind=dp)), pointer :: cost_ptr

C set up 'cost_ptr' to be able to point to different cost functions (Tobler etc.)

cf2py intent (in) costmethod,movetype,bothways,usf,osf,xsize,ysize,numdestinations,maxtime

cf2py intent (in) dem, nullvalue, dx,dy,corridorwidth,timeorenergy

cf2py depend(xsize, ysize) dem, cost, tbnvx, tbnvy

cf2py depend(xsize, ysize) pathx, pathy, visited, incorridor

cf2py depend (movetype) tbnvx, tbnvy

cf2py depend(numdestinations) coords,time, pathx, pathy

cf2py intent (out) cost, time, pathx, pathy, errcode


       print*, 'costmethod ',costmethod
       print*, 'movetype ',movetype
       print*, 'bothways ',bothways
       print*, 'undersample factor ',usf
       print*, 'oversample factor ',osf




       print*, usf,osf
       if (bothways) then
         print*, 'Bothways ',bothways
       end if  

       if (osf.ne.0.0) then
         calculatefine=.True.
       else
         calculatefine=.False.
       end if  

c  if (scalefac-int(scalefac).ne.0) then
c   fac=(scalefac-int(scalefac))*10
c   calculatefine=.True.
c  else
c   calculatefine=.False.
c   fac=0.0
c  end if
c  fac=nint(osf)

c       scaling=int(usf)
C       scalefac is used to hold both scaling and fineness factor
C       in form scaling.fac

       print*, 'initial undersample factor ', usf
       print*, 'CALCULATEFINE', calculatefine
       print*, 'oversample factor = ',osf
       print*, '^^^^^ ',size(tbnvx),size(tbnvy)

c       costmethod=movetype/10
c       movetype=movetype-costmethod*10
c       if (costmethod.ge.5) then
c        costmethod=costmethod-5
c        print*, 'doing it both ways'
c        bothways=.true.
C        calculates an average of walking in both directions
c       else
c        bothways=.false.
c       end if


C       choose appropriate function for cost_ptr to point to....
       if (costmethod.eq.0) then
         critslopeup=0.27
         critslopedown=-0.27
         cost_ptr=> modifiedtobler
        
        
       elseif (costmethod.eq.1) then
         critslopeup=0.29
         critslopedown=-0.29
         cost_ptr=> tobler
        
        
       elseif (costmethod.eq.2) then
         critslopeup=10000.0 !i.e. never achieve critical slope since Naismith does not have one
         critslopedown=-10000.0  
         cost_ptr=> naismith
         
         
       elseif (costmethod.eq.3) then
         critslopeup=0.24
         critslopedown=-0.24
         
         cost_ptr=> minetti
         bothways=.False. !because this is a symmetric function

       elseif (costmethod.eq.5) then
c         critslopeup=0.24
c         critslopedown=-0.24

         critslopeup=0.35
         critslopedown=-0.29
         
         cost_ptr=> carroll

         
       elseif (costmethod.eq.4) then
        cost_ptr=> seaspeed
        critslopeup=10000.0
        critslopedown=-10000.0
        bothways=.False.

       end if


C NB with minetti, cost surface and path are calculated to minimise energy whilst
C time along path is calculated using modified tobler.

C       if (costmethod.eq.5) print*, 'Modified Tobler / Minetti'


       if (movetype.eq.1) print*, 'queen move'
       if (movetype.eq.2) print*, 'Knight move'
       if (movetype.eq.3) print*, 'Rook move'
       if (costmethod.eq.0) print*, 'modifiedtobler'
       if (costmethod.eq.1) print*, 'tobler'
       if (costmethod.eq.2) print*, 'naismith'
       if (costmethod.eq.3) print*, ' minetti'
       if (costmethod.eq.4) print*, 'seaspeed'
       if (costmethod.eq.5) print*, 'Carroll'



       print*, movetype,costmethod,nullvalue

       movex=0
       movey=0

C       if (costmethod.eq.4) corridorwidth=0.0 !since we need to sail around sharp corners

       nummoves=8 !Queen
       if (movetype.eq.3) then !Rook
         nummoves=4
      
       elseif (movetype.eq.2) then !Knight
         nummoves=16
       end if

C Rook
       movex(1)=1;movex(2)=-1;movex(3)=0;movex(4)=0
       movey(1)=0;movey(2)= 0;movey(3)=1;movey(4)=-1
C  + Bishop = Queen
       movex(5)=-1;movex(6)=-1;movex(7)=1; movex(8)=1
       movey(5)=1 ;movey(6)=-1;movey(7)=1;movey(8)=-1

C + Knight
       movex(9) =-1;movex(10)=1;movex(11)=-2;movex(12)=2
       movex(13)=-2;movex(14)=2;movex(15)=-1;movex(16)=1

       movey(9)  =2;movey(10)= 2;movey(11)=1;movey(12) =1
       movey(13)=-1;movey(14)=-1;movey(15)=-2;movey(16)=-2

C
c      dem=0.0!temp test
C             x=coords(1,0)
C             y=coords(1,1)
C              do j=0,ysize-1
C                do i=0,xsize-1
C             dem(i,j)=(((i-x)*dx)**2+((j-y)*dy)**2)**0.5/2.0  !(+/-3.275)
CC i.e. even slope from zero height at origin
C                end do
C              end do
C
C


C       scaling=1
C       if (costmethod.eq.4) then
C        scaling=4
C       end if
       movex=movex*usf !makes it move in bigger jumps, which is OK over the sea since we don't have to worry about slope changes
       movey=movey*usf


       do i=1,nummoves
        distance(i)=((movex(i)*dx)**2+(movey(i)*dy)**2)**0.5
       end do


       print*, (movex(i),i=1,nummoves)
       print*, (movey(i),i=1,nummoves)
       print*, (distance(i),i=1,nummoves)


       bignumber=1.0e12
       errcode=0
C       seaspeed=2.0 !metres per second (3.888 knots)

 10    continue
       repositioned=.False.
       visited=.True.
       incorridor=.False. !cells reserved by corridors set to false
       reached=.False.
C       quadreached=.True.
C quadreached is later set to false for each quadrant within which there is a destination city,
C and again set to true as a city within that quadrant is reached. It is of use in building
C a network, making extra effort to reach places in directions which have not been explored. See
C comments further down under timelimit.

       cost=bignumber
C       time(0)=0.0
       time=0.0

       pathx=0.0
       pathy=0.0



       if (costmethod.ne.4) then
c open up all non-null pixels for visit
       do j=0, ysize-1
         do i=0,xsize-1

           if (dem(i,j).ne.nullvalue) then
             visited(i,j)=.False.

           end if

         end do
       end do
      else

c open up all null pixels for visit (works other way round for sea travel)
        do j=0, ysize-1
          do i=0,xsize-1

            if (dem(i,j).eq.nullvalue) then !use .eq.nullvalue if you want to keep within 50 km of land
              visited(i,j)=.False.

            end if

          end do
        end do


      endif

C      oldcoords=coords
C

C if start or destinations are positioned on a null-vlaue (i.e. out-of-bounds) point
C then find closest in-bounds point and move to that. This necessary e.g. when coastal town
C is on sea point. Also prevents crashing.

C       print*, 'unchanged coords ',coords
C
       unreachable=0
       do k=0,numdestinations

         x=coords(k,0)
         y=coords(k,1)
         print*,'!!!!!!!!!!!!!xypos ',k,x,y
C         pathx(k,1)=x
C         pathy(k,1)=y
         if (visited(x,y)) then
      print*,'repositioning',k,' since start/destination in null value'
           print*, 'changed from ',x,y,' to !!!!!!!!!!!!!!!!'


           mindist=bignumber
           mini=xsize-1
           minj=ysize-1
C set search square to be 4x4 km square around location
           ymarg=2100./dy
           xmarg=2100./dx

C           if (maxtime.eq.999.0) Then
C             ymarg=ymarg*1.5
C             xmarg=xmarg*1.5
C           end if



           do j=max(y-int(ymarg),0), min(y+int(ymarg),ysize-1),3
C     &            max(scaling,3)
             do i=max(x-int(xmarg),0), min(x+int(xmarg),xsize-1),3
C     &            max(scaling,3)

               if (.not.visited(i,j)) then
                 stld=((x-i)*dx)**2+
     &                ((y-j)*dy)**2

C     don't bother to take square roots to save time
                 if (stld.lt.mindist) then
                   mindist=stld
                   mini=i
                   minj=j
                 end if
               end if

             end do
           end do
           coords(k,0)=mini
           coords(k,1)=minj
           repositioned(k)=.True.
C           print*, 'changed coords ',mini,minj
C           print*, visited(coords(k,0),coords(k,1))

C           print*, coords(k,0),coords(k,1),visited(mini,minj)
           if (mindist.eq.bignumber) then !i.e. further than 2 km from non null point
             if (k.eq.0) then !if this is point of origin abort
                errcode=1
                print*, 'point of origin problem'
                return
              else
               unreachable=unreachable+1
C make destination same point as start point so it won't be reached
               coords(k,0)=coords(0,0)
               coords(k,1)=coords(0,1)
               reached(k)=.True.
               pathx(k,0)=999 !flag for this being a null place and not to be chosen again - shortens generation of sea networks significantly
               time(k)=bignumber
               print*,k,'unreachable'
C sets time to destination as very big number
              end if
           end if

C
C           if (numdestinations.lt.1) then !if no valid destinations abort
C             errcode=1
C             return
C           end if


         end if



       end do

       oldcoords=coords
C

       if (unreachable.gt.numdestinations-1) then
c all destinations unreachable
         print*, 'all destinations unreachable'
         errcode=2
         return
       end if



C       print*,'***', dem (coords(0,0),coords(0,1))


       destinationsreached=0


C set start point

       x=coords(0,0)
       y=coords(0,1)
       visited(x,y)=.True.

       print*,'start point, next point ',x,y,coords(1,0),coords(1,1)


C This next part set cells which lie outside corridor as 'visited', assuming width has been
C specified as non-zero - this to increase speed by reducing search area. Note corridor
C width specified in terms of ppn of straight line distance (stld) between start and destination.

       if (corridorwidth.gt.0.0) then


        do k=1,numdestinations

           stld=(((coords(k,0)-x)*dx)**2+((coords(k,1)-y)*dy)**2)**0.5
C stld is the distance along straight line linking start and destination
C m and c are slope and y intercept in st line formula y=mx+c

           ymarg=stld*corridorwidth/4.0


C stld is the distance along straight line linking start and destination
C m and c are slope and y intercept in st line formula y=mx+c
           print*,'corridor width km = ', stld*corridorwidth/1000.0
           realx=real(x)
           if (realx.eq.coords(k,0)) realx=realx+0.0001
c to prevent division by 0
           m=(real(coords(k,1))-real(y))*dy/
     &      ((real(coords(k,0))-realx)*dx)
           c=y*dy-m*realx*dx
C
           do i=mod(x,usf),xsize-1,usf
            do j=mod(y,usf),ysize-1,usf

             if (.not.incorridor(i,j)) then
              c2=j*dy+i*dx/m
              ix=(c2-c)/(m+1.0/m)
              iy=-ix/m+c2
C ix,iy is pos of intercept with baseline of perpendicular through i,j
C  in actual rather than grid space

              dist=((ix-i*dx)**2+(iy-j*dy)**2)**0.5


              dist2=((ix-x*dx)**2+(iy-y*dy)**2)**0.5
C dist is distance in metres between point and intercept perpedicular to baseline
C dist2 is distance in metres between start point and intercept along baseline

               if (dist2.gt.stld+ymarg) dist=bignumber
C
               if (((coords(k,0)*dx-ix)*
     &              (ix-x*dx).lt.0).and. !if intersect lies either side of baseline...
     &               (dist2.lt.stld)) then ! and not beyond destination
                  dist2=ymarg-dist2
                else
                  dist2=dist2+ymarg
                end if
C

                if ((dist2.gt.stld+2*ymarg) .or.
     &                 (dist2.lt.0)) then
                   dist=bignumber
                   dist2=0
                end if


              dist2=dist2/(stld+2*ymarg)*2-1
C    normalised to range  -1.0 .. +1.0
C              ellipsey=max(corridorwidth*(1-dist2**2)**0.5,500.0)
CC             minimum size of ellipsey is 500 m.
C
C
C              if ((dist/stld.gt.ellipsey)) then

               ellipsey=(1-dist2**2)**0.5


               if ((dist/stld.gt.corridorwidth*ellipsey)) then



C if point lies ouside corridor surrounding the line joining start and destination, as
C long as it has not been opened up in another corridor, it is set to visited to prevent inclusion in search
                     visited(i,j)=.True.

                  else
C    otherwise indicate that point lies in corridor so won't be marked as visited when establishing
C    other corridors
                    incorridor(i,j)=.True.
                    if (costmethod.ne.4) then
                      if (dem(i,j).ne. nullvalue) visited(i,j)=.False.
                    else
                      if (dem(i,j).eq. nullvalue) visited(i,j)=.False.
                    end if
                  end if
               end if
             end do
           end do
         end do
       end if


       if (corridorwidth.eq.0.0) incorridor=.True.





       tbnvx=x
       tbnvy=y

       print*, 'heights of start and destinations'

        do i=1,numdestinations
C          print*, 'height and whether visited '
C          print*, dem(coords(i,0),coords(i,1)),
C     &           visited(coords(i,0),coords(i,1))
C          print*, 'x , y distances '
C          print*, (coords(i,0)-x)*dx,(coords(i,1)-y)*dy

          print*, 'Straight-line distances from origin in km ',
     &    (((coords(i,0)-x)*dx)**2+((coords(i,1)-y)*dy)**2)**0.5 /1000.0
C          establishes in which quadrants there are destinations to reach
          deltax=coords(i,0)-x
          deltay=coords(i,1)-y
C          if (deltax>0) then
C           if (deltay>0) then
C             quadreached(1)=.False.
C           else
C             quadreached(2)=.False.
C           end if
C          else
C           if (deltay>0) then
C             quadreached(3)=.False.
C           else
C             quadreached(4)=.False.
C           end if
C          end if


        end do

       cost(x,y)=0.0
       costdir(x,y)=0
       visited(x,y)=.True.
       mincost=0.0
       tbnvcount=0


c make following line live if you want results to be reproduced exactly

        tempcost=rand(6999)


C       print*, 'goto 539'
       do while (((destinationsreached+unreachable<numdestinations)
     &                 .and.(mincost.lt.maxtime)))
C     &             .or.(destinationsreached.eq.0))

C       do while (mincost.lt.bignumber)
C   could save time by removing the mincost stipulation, but this captures when the whole grid has been visited.
C         visited(x,y)=.True.

C         h1=dem(x,y)

C      print*, destinationsreached,unreachable,numdestinations,mincost,
C     &         timelimit
C         print*, 'mincost, maxtime ',mincost, maxtime
C         print*, 'goto 634'


         do i =1 ,nummoves

C (NB don't have to keep new x and y values within bounds of array since we have external
C margin of 'visited' cells)

             newx=x+movex(i)
             newy=y+movey(i)


c not very efficient to put all these if tests in loop but alternative would be cumbersome code
             if (.not.visited(newx,newy)) then

C               ds=((movex(i)*dx)**2+(movey(i)*dy)**2)**0.5
               ds=distance(i)!+(rand()-0.5)/100000.0 !temp test

C
               dh=dem(newx,newy)-dem(x,y)
c  dh=dh+(rand()-0.5)/1.0 !temp test


C following is full slope to test M McHugh
c      dh=( ( (dem(x+1,y)-dem(x-1,y) )/2/dx)**2+
c  &         ( (dem(x,y+1)-dem(x,y-1) )/2/dy)**2)**0.5*ds
C

c                tempcost=ds/speed(dh,ds)/3600.
c                tempcost=cost_ptr(dh,ds)

                if (bothways) then
                  tempcost=(cost_ptr(dh,ds)
     &                +cost_ptr(-dh,ds))/2+cost(x,y)
                else
                  tempcost=cost_ptr(dh,ds)+cost(x,y)
                end if  

              if (tempcost.lt.cost(newx,newy)) then
c                if ((tempcost.lt.cost(newx,newy)) 
c      &  .or.((tempcost.eq.cost(newx,newy)).and.(rand().lt.0.5))) then
 

                   if (cost(newx,newy).gt.bignumber-1) then !new cell

                      tbnvcount=tbnvcount+1
                      tbnvx(tbnvcount)=newx
                      tbnvy(tbnvcount)=newy
                    end if

                   cost(newx,newy)=tempcost
                   costdir(newx,newy)=i !costdir is the backlink matrix, storing the direction from which this cell was reached. 
C     i.e. cost is cumulative time to next cell moving at appropriate speed
C     or existing cumulative time, whichever is smaller.

     

               end if


C              touched-but-not-visited data updated


             end if
         end do

C        print*,'tbnvcount ',tbnvcount

         mincost=bignumber
         mink=3*(xsize+ysize)*movetype
C above line might be unnecessary

C search for lowest cost place that hasn't been visited from which to start again...
C Here we use a maintained list of 'touched-but-not-visited' coordinates. This is added to
C as new cells are given tentative costs and subtracted from as cells are chosen to be visited.
C         print*, 'line 511'
         do k=1,tbnvcount
           costk=cost(tbnvx(k),tbnvy(k))
           if (costk.lt.mincost) then
             mincost=costk
             mink=k
           end if
         end do
C         print*, 'line 519'
C
C         print*,mincost
         if (mincost.eq.bignumber) then
C           this means there is no more room to move, which can happen if start point on an island or lake...
           print*, 'mincost eq bignumber and tbnvcount',tbnvcount
           if (destinationsreached.eq.0) then
C this place will have to be deleted later since it is disconnected from everywhere else
C but could try increasing margin or setting corridorwidth to 0 because may have reached edge of map
             errcode=2
             return
           else
             print*, 'reached dead end so going to paths'
             goto 20
C             finish searching and calculate path(s)
           end if
         end if
C set (x,y) to touched-but-not-visited cell with lowest cost...
         x=tbnvx(mink)
         y=tbnvy(mink)

         visited(x,y)=.True.
C         print*, 'line 540'


         do i = 1, numdestinations
C            if ((x.eq.coords(i,0)).and.(y.eq.coords(i,1))) then
           stld=((x-coords(i,0))**2+(y-coords(i,1))**2)**0.5
           if ((stld.lt.usf*1.4143).and.(.not.reached(i))) then !i.e. within set distance and not already recorded as reached
C i.e. within distance of a diagonal (*1.414) with a little safety margin.

C now reset coords of these destinations to current x,y. Original destination coords still kept in oldcoords and will be accounted for. 
             coords(i,0)=x
             coords(i,1)=y
             destinationsreached=destinationsreached+1
             reached(i)=.True.
             print*, ' reached ',destinationsreached,' of ',
     &          numdestinations,' destinations in ',cost(x,y),'hrs'
             print*, i,'th place'

C             print*, 'x,y,tbnvcount',x,y,tbnvcount
             time(i)=cost(x,y)
C             deltax=coords(i,0)-coords(0,0)
C             deltay=coords(i,1)-coords(0,1)


           end if
         end do

C         print*, 'line 589'


C remove it from list since it will soon be classed as 'visited'
C
         tbnvcount=tbnvcount-1
         k=mink
         do while (k.le.tbnvcount)
           tbnvx(k)=tbnvx(k+1)
           tbnvy(k)=tbnvy(k+1)
           k=k+1
         end do
C
C         print*, mincost, tbnvcount,x,y
C
C       print*, 'line  604'


       end do

       if ((destinationsreached.eq.0).and.(corridorwidth.gt.0)) then
         corridorwidth=corridorwidth+0.1
         print*, 'corridor width ', corridorwidth
         goto 10
       end if
 20    continue

       print*, ' Total of ',destinationsreached,' destinations'

       print*, 'time to destinations is: ',time

C       if (1.lt.1000) then
       print*, 'doing paths....'





C So that path timing calculated retracing path is based on modified tobler.

       if (costmethod.eq.3) cost_ptr=>modifiedtobler

C Now we will create path from backlink matrix. It will be stored backwards. 

       do i=1, numdestinations !destinationsreached


         if ((reached(i)).and.(pathx(i,0).ne.999)) then

           print*, 'path ', i
           count=0
           time(numdestinations+i)=0.0 !i.e. dist(i)
C using this register to store total pathlength

C           if (costmethod.eq.3) then
           temptime=time(i)
           time(i)=0.0 !with minetti recalculate time for route rather than energy if speed=> e.g. modified Tobler
           time(numdestinations*2+i)=0.0 !return time
C           end if
           zag=1
           x=coords(i,0)
           y=coords(i,1)
           mincost=bignumber
C           minj=nummoves+1 !bigger than max num of moves
C NB All paths in same arrays.
           do while (cost(x,y).gt.0.0)
             count=count+1
c costdir is the backlink matrix

             newx=x-movex(costdir(x,y))
             newy=y-movey(costdir(x,y))
C
C
             if (cost(newx,newy).ne.0) then
               k=1
               do while (k.le.nummoves)
                 if (.not.incorridor(newx+movex(k),newy+movey(k)) )then
                  corridorwidth=corridorwidth+0.1
                  print*,'hit edge of corridor '
                  print*, corridorwidth
                  goto 10

                 end if
                 k=k+1
               end do
             end if

             ds =distance(costdir(x,y))

C             print*,cost(x,y)
             dh=dem(x,y)-dem(newx,newy)

             !remember this is actually distance
C             if ((costmethod.eq.3)) then
C         If Minetti energy minisation was used, this quantifies time taken using modifiedtobler.

c               tempcost=cost_ptr(dh,ds) !this is the time taken to go from newx, newy to x,y (we're calculating backwards)
c                tempcost=tempcost*(1.0+(rand()-0.5)/1000000.0)
c                print *,(1.0+(rand()-0.5)/1000000.0)

               time(i)=time(i)+cost_ptr(dh,ds) !latter is the time taken to go from newx, newy to x,y (we're calculating backwards)
               time(numdestinations*2+i)=time(numdestinations*2+i)
     &               +cost_ptr(-dh,ds)
C          latter is return time - used for storing time to walk back along path
C              end if

              time(numdestinations+i)=time(numdestinations+i)+ds !this is a distance. 

              pathx(i,count)=newx
              pathy(i,count)=newy

              x=newx
              y=newy

C           put path length in second half of times


           end do




C           print*, 'ratio is ', time(i)/temptime

           pathx(i,0)=count
           pathy(i,0)=count
C now make sure start and end points are actual start and end - may not be exactly the
C case if using scaling other than 1, so account for any small differences with actual end points
C (stored in oldcoords)

C           if (.not.repositioned(0)) then ! REVISIT THIS...
C             pathy(i,count)=coords(0,1)
C             pathx(i,count)=coords(0,0)
C           end if

C           if (.not.repositioned(i)) then

C Because destination coords were reset to closest square on grid which could be reached
C we set end point of path (1st register) to original coords and increment time / distance
C NB oldcoords set after repositioning. Note that this is done to allow undersampling, whereby it
C may not be possible to land on exactly the right dem square so there is a proximity test. 

             pathx(i,1)=oldcoords(i,0)
             pathy(i,1)=oldcoords(i,1)
             ds=(((coords(i,0)-oldcoords(i,0))*dx)**2
     &              +((coords(i,1)-oldcoords(i,1))*dy)**2)**0.5
             dh=dem(oldcoords(i,0),oldcoords(i,1))-
     &          dem(   coords(i,0),   coords(i,1))

             time(i)=time(i)+cost_ptr(dh,ds)  !totting up time walking to destination
             time(i+numdestinations*2)=time(i+numdestinations*2) !totting up time walking in opposite direction,i.e. from destination to start point. 
     &             +cost_ptr(-dh,ds)


             time(i+numdestinations)=time(i+numdestinations)+ds
C           end if
           print*,'***',costmethod
           print*, 'length of path km',time(i+numdestinations)/1000.0
           print*, 'Recalculated times  ',time(i),
     &           time(i+numdestinations*2)

           print*, 'Average of recalculated oneway times  ',(time(i)+
     &           time(i+numdestinations*2))/2.0
           print*, 'for bothways calculatations this should be very 
     & close to the 2nd element of ^time to destinations^ above. '
           print*, 'Should be close to COST value above but Note that
     & it may be slightly different from the cost value because we have
     & reset the position to precise location'
C         put number of points in path in first element of array

           tempdist=time(numdestinations+i)
           temptime=time(i) !since recomputing cost (i.e. time) as well
           temptimereturn=time(numdestinations*2+i)



          if (costmethod.eq.3) cost_ptr=> minetti
          

          if (calculatefine) then
C next part divides path into segments, e.g. 1000 points each then whatever is left over
C at the end. It then rotates path segment such that end point is on a west-east line through the start point.
C Then generates a rectangular grid around the path segment which is finer than the
C original DEM by a factor of osf. Rotation is to minimize the size of the subgrid
C comtaining the path and thus the computational cost of the second Dijkstra stage,
C also to lessen the potential for path length overestimation (See Carroll and Carroll 2022).
C The subgrid is populated with  interpolated
C DEM values before Dijkstra is rerun to find a least cost path on this
C finer grid. It stores every osf points only, to bring the path resolution
C back to that of the original and to get rid of overestimatation due to chess-board rather than straigth moves.
C It results in a path which allows
C more degrees of freedom in direction changes. E.g. with osf = 5 we can move
C on a 40 point compass rather than the queen's move 8 point compass.
C If critical angle is exceeded (i.e. that at which it becomes cheaper to walk
C further at a shallower angle) every interpolated point is recorded on path.

C Remember that path is stored backwards.
              print*,'CALCULATING SUBGRID!!!!!'

C              if (costmethod.eq.3) speed=> minetti


              time(i)=0.0
              time(numdestinations+i)=0.0
              time(numdestinations*2+i)=0.0


C              fac=4.0
  !  we're creating a grid which is finer by a factor of osf squared (areally) which will ultimately
  ! give angular resolution of ~ 360/8/osf degrees. E.g. osf=5 gives 9 degrees.
  ! Could try larger or smaller value depending on balance between precision and computational expense.


              oldpathx=pathx
              oldpathy=pathy

              seglength=nint(1000.0/(dx**2+dy**2)**0.5)/usf
C          Set segment length to 1000 m


              segstart=1 !segstart is start point of current segment

              ! starts as actual path end point
              kount=0 !kount is total count in fine path position

C              print*,'seglength etc...',seglength,segend


              movex=movex/usf !brings it back to original value
              movey=movey/usf

              do while (segstart.lt.count) !what follows in this loop is unindented
              segend=min(segstart+seglength,count)


              x1=oldpathx(i,segend);x2=oldpathx(i,segstart)
              y1=oldpathy(i,segend);y2=oldpathy(i,segstart)
c x1,y1 are coords of start point. x2,y2 are coords of end point. Remember path stored backwards.


              alpha=atan2((y2-y1),(x2-x1))
C              alpha=0.0 !temp test
              maxx=-bignumber;  maxy=-bignumber
              minx= bignumber;  miny=bignumber

C get grid lengths in rotated x and y directions...
              rdx=((dx*cos(alpha))**2+(dy*sin(alpha))**2)**0.5
              rdy=((dy*cos(alpha))**2+(dx*sin(alpha))**2)**0.5
              rdx=rdx/osf;rdy=rdy/osf
C              print*, x1,y1,' - ',x2,y2
C              print*, rdx,rdy,(rdx**2+rdy**2)**0.5

              do ii=1,nummoves
               subdistance(ii)=((movex(ii)*rdx)**2+
     &                          (movey(ii)*rdy)**2)**0.5
              end do

              beta=0.0 !in case it is not assigned first time around

              do j=segstart,segend
               length=((oldpathx(i,j)-x1)**2+
     &                 (oldpathy(i,j)-y1)**2)**0.5
               if (length.ne.0.0) then
                beta=atan2((oldpathy(i,j)-y1),
     &                     (oldpathx(i,j)-x1))
               end if

               rotatedx=length*cos(beta-alpha)
               rotatedy=length*sin(beta-alpha)
               maxx=max(rotatedx,maxx); maxy=max(rotatedy,maxy)
               minx=min(rotatedx,minx); miny=min(rotatedy,miny)
               if (j.eq.segend) then
C                terminology a bit confusing but path held in reverse order...
                startx=rotatedx
                starty=rotatedy
               elseif (j.eq.segstart) then
                endx=rotatedx
                endy=rotatedy
               end if
              end do

C create an integer margin of about 50 metres...perhaps we don't need it
C but it gives a bit of elbow room.
              xmarg=50./dx!50.0/dx
              ymarg=50./dy!50.0/dy

              maxx=maxx+xmarg;minx=minx-xmarg
              maxy=maxy+ymarg;miny=miny-ymarg
C
C
C              now we need to create a grid of interpolated DEM minx-maxx and miny to maxy


C redefine maxx maxy length and height of grid which covers required area
C The coordinate which fixes this grid is the start point, which is now given by
C startx-minx, starty-miny.

              maxx=int((maxx-minx)*osf)+1
              maxy=int((maxy-miny)*osf)+1
C              minx=0
C              miny=0


C convert start and end points to coords in hi res grid.
              startx=(startx-minx)*osf
              starty=(starty-miny)*osf


              endx=(endx-minx)*osf
              endy=(endy-miny)*osf

C              print*,'start and end points ',startx,starty,endx,endy

C              now create two 1d arrays to define x and y coords from which to interpolate dem
C

              numx=int(maxx+1); numy=int(maxy+1)
C           +1 because e.g. a distance of n is represented by n+1 points.


              no_interp_pts=numx*numy

C              print*,'got to 966'

              ! dynamic allocation of array memory
              allocate ( xcoord((no_interp_pts)))
              allocate ( ycoord((no_interp_pts)))
              allocate ( interpolated_values((no_interp_pts)))


C now define a grid which is what exactly? Explain...

C              print*,'got to 976'


              do j=1,numx
               do k=1,numy
                ix=(j-1)/float(numx-1)*maxx-startx
                iy=(k-1)/float(numy-1)*maxy-starty
C  so point under consideration has (x,y) position relative to path start point of (ix,iy)
C  in rotated coordinates

                length=((ix**2+iy**2)**0.5)/osf
                if (length.ne.0.0) gamma=atan2(iy,ix)

C next couple of lines rotate the points back to get the positions on the original grid
                xcoord((k-1)*numx+j)=length*cos(alpha+gamma)+x1
                ycoord((k-1)*numx+j)=length*sin(alpha+gamma)+y1

               end do
              end do

C              print*,'got to 996'

C              print*,no_interp_pts,size(ycoord),size(xcoord)


C now perform high order interpolation of dem grid onto origins of rotated points
C Last argument 1 for 4th order, 0 for linear

C              print*,'goto to line 1074'
              call interpolate_to_grid(ycoord,xcoord,
     &                 no_interp_pts,interpolated_values,nullvalue)


C               print*,'got to 1079'


C don't need xcoord, ycoord any more so free up memory.
              deallocate (xcoord)
              deallocate (ycoord)


              allocate ( subdem(0:numx-1,0:numy-1))
              allocate ( subcost(0:numx-1,0:numy-1))
              allocate ( subcostdir(0:numx-1,0:numy-1))
              allocate ( subvisited(-10:numx+9,-10:numy+9))

C              print*,'got to 1010'


              do j=0,numx-1
               do k=0,numy-1

                subdem(j,k)=interpolated_values(k*numx+j+1)

               end do
              end do

              deallocate (interpolated_values)

              x=nint(startx)
              y=nint(starty)

              subvisited=.True.
C set to true first because we want the values outside the 0-numx-1 range to be True
C where we don't have nullvalues
              subcost=bignumber

              if (costmethod.ne.4) then

                do j=0, numy-1
                  do ii=0,numx-1

                    if (subdem(ii,j).ne.nullvalue) then
                      subvisited(ii,j)=.False.

                    end if

                  end do
                end do

              else

                do j=0, numy-1
                  do ii=0,numx-1

                    if (subdem(ii,j).le.nullvalue) then
                      subvisited(ii,j)=.False.

                    end if

                  end do
                end do
               end if

C so now start and end points have been rotated to allow smallest surrounding subgrid.
C These points on the grid have been populated by interpolated dem values at osf times greater resolution.
C All ready to run Dijkstra on....

              subcost(x,y)=0.0
              subcostdir(x,y)=0
              subvisited(x,y)=.True.
C              mincost=0.0
              tbnvcount=0

              mincost=bignumber-1
              stilllooking=.True.


C              print*,'x,y,endx,endy',x,y,endx,endy
              do while ((stilllooking).and.(mincost.lt.bignumber))
C                print*, '*************&*&*&*&*',mincost

C       do while (mincost.lt.bignumber)
C   could save time by removing the mincost stipulation, but this captures when the whole grid has been visited.
C         visited(x,y)=.True.

C         h1=dem(x,y)

C      print*, destinationsreached,unreachable,numdestinations,mincost,
C     &         timelimit

                do ii =1 ,nummoves

C (NB don't have to keep new x and y values within bounds of array since we have external
C margin of 'visited' cells)

                  newx=x+movex(ii)
                  newy=y+movey(ii)
C                  print*,movex(ii),movey(ii)

c not very efficient to put all these if tests in loop but alternative would be cumbersome code
                  if (.not.subvisited(newx,newy)) then
C                    print*,'subvisited ',x,y,endx,endy


C               ds=((movex(i)*dx)**2+(movey(i)*dy)**2)**0.5
                    ds=subdistance(ii)

C
                    dh=subdem(newx,newy)-subdem(x,y)
c tempcost=cost_ptr(dh,ds)

c                   tempcost=tempcost*(1.0+(rand()-0.5)/1000000.0)
C# i.e. random perturbation of plus or minus 5 in 10 million

                    if (bothways) then
                      tempcost=(cost_ptr(dh,ds)
     &                     +cost_ptr(-dh,ds))/2+subcost(x,y)
                    else
                      tempcost=cost_ptr(dh,ds)+subcost(x,y)
                    end if  


C                print*, 'line 482'


                    if (tempcost.lt.subcost(newx,newy)) then
                     if (subcost(newx,newy).gt.bignumber-1) then !new cell

                      tbnvcount=tbnvcount+1
                      tbnvx(tbnvcount)=newx
                      tbnvy(tbnvcount)=newy
                     end if

                     subcost(newx,newy)=tempcost
                     subcostdir(newx,newy)=ii
C     i.e. cost is cumulative time to next cell moving at Tobler/Naismith speed
C     or existing cumulative time, whichever is smaller.

                    end if

C              touched-but-not-visited data updated


                  end if !(.not.subvisited(newx,newy)) then
              end do !ii =1 ,nummoves

C        print*,'tbnvcount ',tbnvcount

              mincost=bignumber
              mink=3*(xsize+ysize)*movetype
C above line might be unnecessary

C search for lowest cost place that hasn't been visited from which to start again...
C Here we use a maintained list of 'touched-but-not-visited' coordinates. This is added to
C as new cells are given tentative costs and subtracted from as cells are chosen to be visited.
C         print*, 'line 511'
              do k=1,tbnvcount
                costk=subcost(tbnvx(k),tbnvy(k))
                if (costk.lt.mincost) then
                  mincost=costk
                  mink=k
                end if
              end do
C         print*, 'line 519'

C         print*,mincost
              if (mincost.eq.bignumber) then
                print*,'mincost.eq.bignumber in subgrid'
                print*,'suggests destination unreachable (e.g. island)'
                print*,'Reverting to coarse path...'

                deallocate (subdem)
                deallocate (subcost)
                deallocate (subcostdir)
                deallocate (subvisited)
                time(i)=temptime
                time(numdestinations+i)=tempdist
                time(numdestinations*2+i)=temptimereturn

                pathx=oldpathx;pathy=oldpathy
                movex=movex*usf !brings it back to scaled value
                movey=movey*usf


                goto 30




              end if
C set (x,y) to touched-but-not-visited cell with lowest cost...
              x=tbnvx(mink)
              y=tbnvy(mink)

              subvisited(x,y)=.True.
C              print*, x,y,endx,endy

C             print*,x,y,endx,endy


C            if ((x.eq.endx).and.(y.eq.endy)) then
             stld=((x-endx)**2+(y-endy)**2)**0.5
             if ((stld.lt.1.0)) then!  .and.(.not.reached(i))) then !i.e. within set distance and not already recorded as reached
              endx=x
              endy=y
C              print*,'found place on hi res subgrid, cost=',subcost(x,y)

C              time(i)=subcost(x,y) !this is time before kinks have been smoothed by filtering
                                 ! in fact not necessary to assign

              stilllooking=.False.
             end if

C         print*, 'line 589'


C remove it from list since it will soon be classed as 'visited'
C
             tbnvcount=tbnvcount-1
             k=mink
             do while (k.le.tbnvcount)
               tbnvx(k)=tbnvx(k+1)
               tbnvy(k)=tbnvy(k+1)
               k=k+1
             end do
C
C         print*, mincost, tbnvcount,x,y
C
C       print*, 'line  604'


           end do !((stilllooking).and.(mincost.lt.bignumber))

           subcount=0

C So that path timing calculated retracing path is based on modified tobler.

          if (costmethod.eq.3) cost_ptr=> modifiedtobler


C           tempdist=time(numdestinations+i)
C           temptime=time(i) !since recomputing cost (i.e. time) as well
C using this register to store total pathlength. Now reset to 0...
C           time(i)=0.0
C           time(numdestinations+1)=0.0

           x=nint(endx)!end point of path
           y=nint(endy)


           lastx=x
           lasty=y

           mincost=bignumber
C           count=0

C           minj=nummoves+1 !bigger than max num of moves

           do while (subcost(x,y).gt.0.0)
             subcount=subcount+1

             newx=x-movex(subcostdir(x,y))
             newy=y-movey(subcostdir(x,y))

             ds=subdistance(subcostdir(x,y))

C
             dh=subdem(x,y)-subdem(newx,newy)


C next part, including if statement ensures that only every osf th point on path is
C recorded, thus smoothing all the queen's move twists and turns and bringing the
C spatial resolution back to an equivalent to that of the original grid before subsampling.
C However, extra directional resolution is retained at close to 45/osf degrees.
C Every point on the subgrid is recorded if some critical slope is exceeded.
C This allows rapid directional switching associated with hairpin bends on steep slopes.
C Then transform back to non-rotated coords on original grid to store path.


C need to look at critslopeup and down again given that the path may be created with bothways = True.
C In this instance the easiest thing would be to define a different critical slope. Not a problem with 
C Tobler or modified Tobler since the up and down critical slopes have the same absolute magnitude. 

             if ((mod(subcount,int(osf)).eq.0) .or.
     &                 ((dh/ds.gt.critslopeup))  .or.
     &                 ((dh/ds.lt.critslopedown))  .or.
     &                 (subcost(newx,newy).eq.0) ) then

C   i.e. record every facth point on path, all points when critical grdient exceeded, or when
C  newx,newy are last point.
C               print*,subcount,fac
               kount=kount+1 !kount is position in total path
C               print*,'kount=',kount
               length=((newx-startx)**2+(newy-starty)**2)**0.5
C               note that this is length in grid space rather than real length
               length=length/osf
               subcount=0
               gamma=atan2((newy-starty),(newx-startx))
               pathx(i,kount)=length*cos(alpha+gamma)+x1
               pathy(i,kount)=length*sin(alpha+gamma)+y1



               ds=(((lastx-newx)*rdx)**2+
     &             ((lasty-newy)*rdy)**2)**0.5


               dh=subdem(lastx,lasty)-subdem(newx,newy) !remember, doing this backwards

c               tempcost=cost_ptr(dh,ds)
c               print*, 'dist ', ds

               time(i)=time(i)+cost_ptr(dh,ds)
               time(i+numdestinations*2)=time(i+numdestinations*2)
     &               +cost_ptr(-dh,ds)

               time(i+numdestinations)=time(i+numdestinations)+ds

               lastx=newx
               lasty=newy

             end if !((mod(subcount,...

             x=newx
             y=newy

           end do !while (subcost(x,y).gt.0.0)...

           deallocate (subdem)
           deallocate (subcost)
           deallocate (subcostdir)
           deallocate (subvisited)

C           print*, 'ratio is ', time(i)/temptime

C           pathx(i,0)=kount
C           pathy(i,0)=kount

C         end if



C We have filtered the path by only taking every fac points. So now we need to recalculate
C both time cost and distance along this path which has had queen's move kinks ironed out.
C


        segstart=segend
        end do !new do while (segstart.lt.count) - i.e. finished adding segment and on to next

        movex=movex*usf !brings it back to scaled value
        movey=movey*usf

C         pathx(i,0)=kount
C         pathy(i,0)=kount
C        end if !calculatefine
C  put number of points in path in first element of array

C        if (calculatefine) then !dont need this if region?
C
         pathx(i,0)=kount
         pathy(i,0)=kount

C        end if

C now work out distance a second way - on original grid.
C         dist2=0
C         do j=1,kount-1
C           dist2=dist2+(((pathx(i,j+1)-pathx(i,j))*dx)**2+
C     &                  ((pathy(i,j+1)-pathy(i,j))*dy)**2)**0.5
C
C         end do

 30      continue !sent here if failure to find subgrid route....

         print*, '---- FINE recalculated time along path is ',time(i)
         if (time(i).lt.0.001) then
           time(i)=temptime
           time(i+numdestinations*2)=temptimereturn
           time(i+numdestinations)=tempdist
           pathx=oldpathx;pathy=oldpathy
           print*,'resetting to coarse time - fine too low',temptime
         end if

         print*, pathx(i,0),'pathththth'

C        print*,'number of point in path ',pathx(i,0)
C
         print*, 'length of path km',time(i+numdestinations)/1000.0
C         print*, dist2/1000.0
         timeratio=time(i)/temptime*100
         distratio=time(i+numdestinations)/tempdist*100
         print*,'time and return time',
     &          time(i),time(i+2*numdestinations)

         print*, 'Distance Roman miles ',
     &    time(i+numdestinations)*.6214/.92/1000.0
         print*,timeratio,distratio !,dist2/tempdist*100

C         if (costmethod.eq.3) speed=>minetti


        end if !calculatefine

        end if !((reached(i)).and.(pathx(i,0).ne.999)) then
       end do !i=1, numdestinations


C      end if !doing paths

       print*,'time and dists ',time




       return


       contains


        real(kind=dp) function tobler(dh,ds)
        real(kind=dp) dh,ds,speed
        speed=6.0/3.6*exp(-3.5*abs(dh/ds+0.05))
        tobler=ds/speed/3600.
        end function


       real(kind=dp)function modifiedtobler(dh,ds)
C see Marquez-Perez et al., 2017
       real(kind=dp) dh,ds,speed
C       print*,'modifiedtobler'

       speed=4.8/3.6*exp(-5.3*abs(0.7*dh/ds+0.03))
               

c       division by 3.6 to covert from km/hr to m/s
       modifiedtobler=ds/speed/3600.

       end function



       real(kind=dp)function carroll(dh,ds)

       real(kind=dp) dh,ds,speed,asquared,Smax,n,vmax
C       print*,'modifiedtobler'
c following tuned to give crit slope 0.24
c  asquared=0.054351
c  Smax=-0.057
c  n=2.0
c  vmax=5.3

c following based in GPS study of my own walking 

       asquared=0.0961
       Smax=-0.057
       n=1.65
       vmax=5.2

       speed=vmax/3.6*asquared/((dh/ds-Smax)**n+asquared)
       carroll=ds/speed/3600
       end function









       real(kind=dp)function seaspeed(dh,ds)

       real(kind=dp) dh,ds,speed
C       print*,'seaspeed'

       speed=2.0
      

c      if (ds.gt.30.0) then

c        seaspeed=seaspeed*(1.0+(rand()-0.5)/10.0)
       speed=speed*(1.0+(rand()-0.5)/1000.0)
       seaspeed=ds/speed/3600. !this is really a time rather than a speed

c      end if
C base assumption of ship speed is 2 metres per second (3.888 knots)
C ds is dummy variable but can be used as flag to apply random perturbation of +/- 0.005 percent
C for other than small gridlengths - i.e. not when doing fine, rotated grid.
       end function


      real(kind=dp) function naismith(dh, ds)
         real(kind=dp), intent(in) :: dh, ds

c         ! This function implements Naismith's rule with Langmuir's corrections
c         ! for downhill speed. It calculates travel time in hours.

c         ! More accurate conversion from Naismith's original imperial measurements
c         ! (304.8 m per 1000 ft, 3 mph = 4.828 km/h)
         if (dh / ds > 0) then
c           ! Uphill: Naismith's rule
           naismith = ds*(1/4.828+5./3.048*dh/ds)/1000.
         elseif (dh/ds>-0.08749) then
           ! Flat terrain: constant speed of 4.828 km/h
           naismith=ds/4.828/1000.
         elseif (dh/ds>-0.21256) then
           ! Gentle downhill: Langmuir correction for moderate negative slope
           naismith=ds*(1/4.828+1.0/(.3048*6.0)*dh/ds)/1000.0
         else
           ! Steep downhill: Langmuir correction for steeper negative slope
           naismith=ds*(1/4.828-1.0/(.3048*6.0)*dh/ds)/1000.
         end if

      end function



      real(kind=dp) function minetti(dh,ds)
      real(kind=dp) dh,ds,g
c     &  ,g2,g3,g4,g5

      g=dh/ds

c remember to reinstate each way method and allow for the fact that Minetti's speed are parallel to the treadmill

C the following derived by adding the costs for each of Minetti's down and uphill values of metabolic cost, 
c e.g. costs for -0.1 + 0.1, costs for -0.2 + 0.2 etc. to give total cost for walking in both directions, and
c fitting to 2nd degree polynomial, weighting points by combined standard deviation. Therefore implicitly a 'both ways' function. 
c minetti=ds*(20.53453131*g**2+ 10.50873313*g + 1.54959331)/6240.0 !6240.0 is simply a weighting function to make the metabolic cost similar to time

C based on fitting to minetti data corrected to give horizontal distances and up+down values added and regressed to 2nd order curve
C giving 0.9989 R squared. Note this is an inherently bothways method (Symmetric function). 

      minetti=ds*(1.5648 + 9.8117*g + 26.4603*g**2)/5000.0


      end function




C***11111111111111111111111111111111111111111111111111111111111111111111
C
C
C FILE NAME: morph_interpolate_to_grid.f
C
C TYPE: subroutine
C
C LANGUAGE: FORTRAN

       subroutine interpolate_to_grid
     &      (ycoord,xcoord,no_interp_pts,interpolated_values,nullvalue)

! 4th order part commented out due to little difference in result.
! This works by first calculating slope (dbydx, dbydy) of field at each point. It then uses
! the slope (2nd stage) to extrapolate values from nearest grid point on one side in the
! x direction and from nearest on the other in the x direction, making a
! weighted mean of the two according to the interpolation point's distance from
! grid point. Same process in y direction before combining the two. If slope
! is set to zero (as at edges) this results in simple bilinear interpolation. Away from
! edges and null values a centred, second order accurate slope expression is used.
! If next to null value, this is not used and a first-order expression is used for slope.

!**************
! You can make an argument for simple bilinear on the basis that then the slope at each
! interpolated point is closest to the grid-scale slope used in the cost surface calculation,
! which is only a simple, first-order accurate one based on the difference in height between
! start and destination points. In which case maybe the whole routine should be simplified
! with no slope calculation for interpolation.
!**************

! Second part is the interpolation and if a null value lies on any of the surrounding
! points it simple takes the nearest value.


! NB if start/end point of route segment near shores of elevated lake, scope for problems
! due to interpolation between large elevation datum and the minus 9, the null value given to
! water surfaces. endx,endy and startx,starty are rounded onto subgrid integer positions
! so don't line up completely. This leads to spuriously high costs.
! Solution would be to stop the interpolation routine
! from using the null value and making sure start and end are on non-null points in subgrid.


      implicit none

C** argument vrbles

      integer
     &      no_interp_pts   !no. of pts on interpolated grid



C      real dem(M,N), !v1.1 was: (129, 129) values to be interpolated
      real (kind=dp)    interpolated_values(no_interp_pts)


C** local vrbles

      real (kind=dp)
     &   xinterpA,xinterpB,xinterp1,xinterp2, !intermediate interpolated values
     &   interp1,interp2, !intermediate interpolated values
C     &   YslopeinterpA,YslopeinterpB, !interpolated dbydy values
C     &   dbydy(0:xsize-1,0:ysize-1),! v1.1 was (129,129) 1st derivative of dem in y dir'n
C     &   dbydx(0:xsize-1,0:ysize-1), !v1.1 was (129,129) 1st derivative of dem in x dir'n
     &   xcoord(no_interp_pts),ycoord(no_interp_pts),
     &   deltax,deltay, !non-integer parts of x and y coords
     &   nullvalue

      integer
     & i, !loop counters
     & intx,inty !integer parts of coords of new points
C     & flag ! 0 for 2nd order, 1 for 4th order
C

C
        do i=1,no_interp_pts


         intx=int(xcoord(i))
         inty=int(ycoord(i))

         intx=min(intx,xsize-2)
         inty=min(inty,ysize-2)



         if  (min(dem(intx+1,inty),dem(intx,inty+1), !NB nullvalue needs to be lower than any dem
     &         dem(intx+1,inty+1),dem(intx,inty)).eq.nullvalue) then

            interpolated_values(i)= dem(nint(xcoord(i)),nint(ycoord(i)))


         else
C


          deltax=xcoord(i)-intx
          deltay=ycoord(i)-inty

          xinterp1=dem(intx,inty)!+deltax*dbydx(intx,inty)
          xinterp2=dem(intx+1,inty)!-(1.-deltax)*dbydx(intx+1,inty)
          xinterpA=xinterp1*(1.-deltax)+xinterp2*deltax
C          yslopeinterpA=dbydy(intx,inty)*(1.-deltax)
C     &                +dbydy(intx+1,inty)*deltax

          xinterp1=dem(intx,inty+1)!+deltax*dbydx(intx,inty+1)
          xinterp2=dem(intx+1,inty+1)!-(1.-deltax)*dbydx(intx+1,inty+1)
          xinterpB=xinterp1*(1.-deltax)+xinterp2*deltax
C          yslopeinterpB=dbydy(intx,inty+1)*(1.-deltax)
C     &                +dbydy(intx+1,inty+1)*deltax

          interp1=xinterpA!+yslopeinterpA*deltay
          interp2=xinterpB!-yslopeinterpB*(1.-deltay)
          interpolated_values(i)=interp1*(1.-deltay)+interp2*deltay

         end if
        end do


      return


      end subroutine













      end
