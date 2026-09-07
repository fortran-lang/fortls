c Implicit main program: no PROGRAM statement, fixed form.
c The bare END must close the implicit main scope so that the
c following top-level units are parsed as external procedures.
      integer i
      i = 1
      end
      subroutine calc(i)
      integer i
      i = i + 1
      end
      function dbl(x)
      dbl = 2 * x
      end
