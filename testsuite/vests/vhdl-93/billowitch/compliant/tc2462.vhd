
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- ---------------------------------------------------------------------
--
-- $Id: tc2462.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p03n02i02462ent IS
END c07s03b02x02p03n02i02462ent;

ARCHITECTURE c07s03b02x02p03n02i02462arch OF c07s03b02x02p03n02i02462ent IS
  type    UNCONSTRAINED_ARRAY    is array ( integer range <> ) of character;
  subtype CONSTRAINED_ARRAY    is UNCONSTRAINED_ARRAY ( 1 to 3 );
  type    AGGREGATE_ARRAY    is array (1 to 2) of CONSTRAINED_ARRAY;
  signal  V, W         :  CONSTRAINED_ARRAY;
BEGIN
  TESTING: PROCESS
  BEGIN
    (W,V) <= (AGGREGATE_ARRAY'((others => '$'),( others => '$' )));
    wait for 1 ns;
    assert NOT( V(1)='$' and V(2)='$' and V(3)='$' and W=(('$','$','$')))
      report "***PASSED TEST: c07s03b02x02p03n02i02462"
      severity NOTE;
    assert ( V(1)='$' and V(2)='$' and V(3)='$' and W=(('$','$','$')))
      report "***FAILED TEST: c07s03b02x02p03n02i02462 - An array aggregate with an others choice may appear as a value expression in an assignment statement."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p03n02i02462arch;
