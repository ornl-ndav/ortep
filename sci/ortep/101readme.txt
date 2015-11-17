ORTEP-III Version 1.0.1 (Oct. 1, 1996) adds the following capabilities not
found in Version 1.0 (Apr. 1, 1996).

        * Dashed Bonds (for hydrogen bonds)
        * Line Bonds without Atom Symbols (for wireframe structures)

** Dashed Bonds **

Dashed bonds may be produced with the 801/811 and 802/812 instructions
by setting the value in the "bond type" field (cols. 22-24) on the
Format No. 2 cards to a number in the range 10 through 999 or 
-99 through -10.

The leftmost two digits indicate the number, m, of dashes to draw.
(The number of spaces is m-1.)  The rightmost digit, n, indicates the
size of the dashes relative to the size of the spaces between the
dashes.  The ratio of dash length to space length is n/(10-n).  This
is illustrated in the examples below.

        bond type       number  number  
          value         dashes  spaces  dash length/space length

           154            15      14              4/6
           128            12      11              8/2
           101            10       9              1/9
            95             9       8              5/5 = 1
            40             4       3               1
            10             1       0               -

   Notes:

   If the rightmost digit is greater than 5, dashes are longer than
   spaces.

   If the rightmost digit is less than 5, spaces are longer than
   dashes.

   If the rightmost digit is 0 or 5, dashes and spaces are the same
   length.

   The rightmost digit 1 can be used to produce lines that appear
   dotted rather than dashed (if the number of dashes is sufficiently
   large).

   The "bond type" value for dashed bonds may be negative.  [See the
   ORTEP-III manual (Section 3.3.9) for the difference between negative
   and positive "bond type" values.]  However, including a negative
   sign limits the number of dashes that can be drawn to 9 since the
   "bond type" card field is only three characters wide.

The width of dashed bonds is controlled by the value in the "bond
radius" field (cols. 37-42) on the Format No. 2 cards (exactly as for
non-dashed bonds).  Supplying a small non-zero value will result in the
bond appearing as a single line rather than two.

The spaces in dashed bonds are not transparent (i.e., lines behind the
dashed bonds do not show through the spaces).

As always, the No. 2 cards of the overlap correction instructions 821,
822, and 1001 should match those of the 800 instructions.

** Line Bonds without Atom Symbols **

The 803/813 instructions in Version 1.0 draw the structure with the
asterisk (*) character on atom sites with single line bonds connecting
them.  In Version 1.0.1, the character on the atom site can be
eliminated by providing any non-zero integer value in columns 22-24
on the Format No. 2 cards of the 803/813 instructions.

   Note:

   Structures drawn with 803/813 are not corrected for overlap.
