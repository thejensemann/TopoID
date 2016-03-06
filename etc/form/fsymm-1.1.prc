#ifndef `FSYMM'
  #define FSYMM

************************************************************************
* fsymm(<list_of_symmetric_(sub-)topologies>)
*
* Acts on integrals in factor representation, i.e.
*   d1^2*d2^1*...,
* via, e.g., the call
*   #call fsymm(d1\\\,d2\,d1\\\,d3),
* meaning (d1,d2) with no (d3) is equivalent to (d1,d3) with no (d2).
*
* Consider each symmetry element as base:
* - find vanishing factor symbols and generate matching condition to
* - try replacements into all symmetry elements.
*
* Needs:
*   #define top "<top>"
*   #define d`top' "<d_1>,...,<d_n>"

#procedure fsymm(symms)
  #do base = {`symms'}

* --- find vanishing factors -------------------------------------------

    #undefine zeros
    #do d = {`d`top''}
      #undefine match`d'
      #do b = {`base'}
        #if `d' == `b'
          #define match`d'
          #breakdo
        #endif
      #enddo
      #ifndef `match`d''
        #ifndef `zeros'
          #define zeros "`d'"
        #else
          #redefine zeros "`zeros',`d'"
        #endif
      #endif
    #enddo

    #do symm = {`symms'}

* --- find replaced symbols --------------------------------------------

      #undefine args
      #define i "1"
      #do s = {`symm'}
        #define j "1"
        #do b = {`base'}
          #if (`i' == `j') && (`b' != `s')
            #ifndef `args'
              #define args "`b',`s'"
            #else
              #redefine args "`args',`b',`s'"
            #endif
            #breakdo
          #endif
          #redefine j "{`j' + 1}"
        #enddo
        #redefine i "{`i' + 1}"
      #enddo

* --- generate matching condition, replacement command -----------------

      #ifdef `args'
        #ifdef `zeros'
          if(1
          #do zero = {`zeros'}
            && (count(`zero',1) == 0)
          #enddo
          )
        #endif
        tryreplace `args';
      #endif

    #enddo

  #enddo
#endprocedure

************************************************************************

#endif
