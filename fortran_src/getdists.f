      subroutine getdists
     &(distarrayFG,numplaces,lat,lon,stldarray)


      implicit none

      integer, parameter :: dp = selected_real_kind(15, 307)

      integer numplaces,i,j,k,arrayk(1),count

       real(kind=dp) distarrayFG(0:numplaces-1,0:numplaces-1),
     &         stldarray(0:numplaces-1,0:numplaces-1),
     &         stldarraytemp(0:numplaces-1,0:numplaces-1),
     &         lat(0:numplaces-1),lon(0:numplaces-1),dy,
     &         avspeed(0:numplaces-1)


       real(kind=dp) pi/3.1415926/

cf2py intent (out) stldarray
cf2py intent (in,out) distarrayFG
cf2py intent(in) numplaces,lat,lon,avspeed
cf2py depend(numplaces)lat,lon,distarrayFG,stldarray



      do i=0,numplaces-1
       dy=cos(lat(i)/180.0*pi)
       do j=0,numplaces-1
C could substitute for more accurate expression but this will do...
        stldarray(i,j)=111.0*((dy*(lon(i)-lon(j)))**2+
     &                         (lat(i)-lat(j))**2)**0.5



       end do
      end do



C next part finds unconnected places. For each, it finds the closest place which is not unconnected
C and estimates the finite distarrayFG values from this, weighted by ratio of distances
      stldarraytemp=stldarray


C
      do i=0,numplaces-1
        if (minval(distarrayFG(i,:)).gt.1e19) then
      ! place i is unconnected - perhaps because it does not occur in supplied distarrays

          count=0
 10       continue
          arrayk=minloc(stldarraytemp(i,:))!find j value of closest place to i for which we have valid dists (times)
          k=arrayk(1)-1 !convert from array to integer and subtract 1 because distarray runs from 0
          if (minval(distarrayFG(k,:)).gt.1e19) then !if this place (k) is also unconnected, look for next closest
            stldarraytemp(i,k)=1e19
            count=count+1
            if (count.gt.5) goto 20 !apalling style
            goto 10
          else
            do j=0,numplaces-1
              if (distarrayFG(k,j).lt.1e19) then
               distarrayFG(i,j)=distarrayFG(k,j)
     &               *stldarray(i,j)/stldarray(k,j)
               distarrayFG(j,i)=distarrayFG(i,j)

              end if
            end do
         end if

        end if

      end do

 20   continue

      do i=0,numplaces-1
       avspeed(i)=0
       count=0
       do j=0,numplaces-1
        if (distarrayFG(i,j).lt.1e19) then
         count=count+1
         avspeed(i)=avspeed(i)+stldarray(i,j)/distarrayFG(i,j)
        end if
       end do
       if (count.gt.0) then
         avspeed(i)=avspeed(i)/count
       else
        avspeed(i)=3.4
       end if
      end do

      do i=0,numplaces-1
       do j=0,numplaces-1
         if (distarrayFG(i,j).gt.1e19) then
          distarrayFG(i,j)=stldarray(i,j)/avspeed(i)
         end if
       end do
      end do


      return

      end subroutine getdists
