
      subroutine dijkstraspecial
     &     (distarray, numnodes,totdistarray)

C Find shortest paths using Dijkstra's algorithm. Inputs are simply the number of nodes
C an a distance array of numnodes x numnodes, where each element N(i,j) is the distance
C from node i to node j. Where there are no connections, N(i,j) is set to infinity or at least
C a very large number.
C Output is totdistarray which contains the shortest distances from each node to every other node.
C Note that this works both with symmetrical, isotropic arrays, where N(i,j) = N (j,i)
C and with anisotropic arrays where this does not necessarily hold because e.g. of sloping
C routes which are quicker in one direction than another.
C This version maintains a list of 'touched but not visited' places
C which speeds up search for next nearest place somewhat. Also excludes nodes from which all values
C are large by setting visited to 'true'.

       implicit none

       integer numnodes

       real*8 distarray(0:numnodes-1,0:numnodes-1),
     &        totdistarray(0:numnodes-1,0:numnodes-1),
     &        bignumber

       logical visited(0:numnodes-1)

       integer startcity,currentcity,k,tbnvcount,
     &               mink,tbnv(numnodes*numnodes)


       real*8 mindist, currentdist, tempdist

cf2py intent (in) numnodes,distarray

cf2py depend(numnodes) distarray,totdistarray,tbnv

cf2py intent(out) totdistarray

       bignumber=1.0e20 !this was 1.0e19 but this caused a crash in certain circumstances
       print*,numnodes, 'got to 40'
       totdistarray=bignumber

c set diagonal equal to zero since a node is zero distance from itself.
       do k= 0,numnodes-1
         totdistarray(k,k)=0.0
       end do

       print*,numnodes,'got to 47'

       do startcity = 0 , numnodes-1

C         print*,'Start city ',startcity
C
         visited=.false.
C         do k= 0,numnodes-1
C           if (totdistarray(startcity,k).gt.100000.0) visited(k)=.true.
C         end do


         currentcity=startcity !currentcity is the next node in the quest for the shortest route
         currentdist=0.0
         mindist=0.0
         tbnvcount=0

c  print*,numnodes,'got to 64'


         do while (mindist.ne.bignumber)


           do k = 0, numnodes-1

            if (.not. visited(k)) then

              tempdist=currentdist+distarray(currentcity,k)
              if (tempdist.lt.totdistarray(startcity,k)) then
                if (totdistarray(startcity,k).gt.1.0e19) then !new place
                  tbnvcount=tbnvcount+1
                  tbnv(tbnvcount)=k
C                  print*, tbnvcount
                end if


                totdistarray(startcity,k)=tempdist






              end if

            end if

           end do

           visited(currentcity)=.true. ! to tick this city off as 'done'

c          print*,numnodes,'got to 98'


C Problem here is that mindist will have to be recomputed from amongst all the unvisited nodes.
C Maybe we need to store a set of nodes for which tentative distances have been computed but
C which have not been 'visited', as I implemented in calculatecost.f

C
C do k=1,tbnvcount
C  costk=cost(tbnvx(k),tbnvy(k))
C  if (costk.lt.mincost) then
C    mincost=costk
C    mink=k
C  end if
C end do
C





           mindist=bignumber
c  print*,numnodes,'got to 120',tbnvcount


           do k=1,tbnvcount


               if (totdistarray(startcity,tbnv(k)).lt.mindist) then
                 mindist=totdistarray(startcity,tbnv(k))
C                 currentcity=tbnv(k)
C                 currentdist=mindist
                 mink=k
c                print*,k,'got to 131'

               end if
           end do
           currentcity=tbnv(mink)
           currentdist=mindist

c          print*,numnodes,'got to 134',mindist

           tbnvcount=tbnvcount-1
           k=mink
           do while (k.le.tbnvcount)
             tbnv(k)=tbnv(k+1)
             k=k+1
           end do




         end do

       end do


       return

       end
