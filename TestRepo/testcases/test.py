#!/usr/bin/python

import os
from subprocess import call

for filename in os.listdir("testcases"):
  if (filename[-4:] == ".txt"):
    print "---------------------------------------------------------"
    print "File " + filename + ":" #+ str(output1 == output2)

    print "----running Haskell----"
    call(["./assignment2/main","testcases/" + filename, "output1.txt"])

    print "----running Prolog-------"
    call(["./assignment3/solver","testcases/" + filename, "output2.txt"])

    output1 = open("output1.txt", 'r').read()
    output2 = open("output2.txt", 'r').read()
#    output2 = open("output2.txt", 'r').read()
    print "----RESULT-------------"
    print output1.strip()
    print output2.strip()
#                                          ,~~~~~~~~~~~~~~~~~~~~,
#                                        ,'   You guys!! What    ',
#                                        |    does that code      |
#                                        ',   even MEAN?         ,'
#                                          '~~~~~~.,   ,.~~~~~~~'
#                                                  |  /
#                                                  | /
#                                                  |/
#                                                  '
#                                           .~-,
#                                         .' `,>
#                                      .-'   ,>  <7`,
#                                    ,`     ,>  <7  }
#                                   {   o   !> <7  /
#                                   {       j_7`  !
#                                   :            j'
#                                    `,         ]
#                                     F        }
#                                    F       , {
#                                   F         `.   rr
#                                 .F          | `=-"
#       _,-`                    .F            `,
#     ,`;                     .F               j
#    :  7                    F                 ;
#    :  `^,                F`                 ,'
#    `,    `^,          ,F`     .          ,  /
#      `,     `^-^-^-^-`       ;           | ;
#        `.                    :          .`/      ,-^,-
#          `-.                 }         ,'' `,_.-^ /'
#             `-,___           ;      .'` -_      ,7
#                   ``=-....-={     ,/      `-','`Q
#                              \     |                .-'-.]
#                               `,   `.             .`  0  `.
#                                `,   l_           `|  __   |`
#                               .C.-,___`==,        |  ||   |

#CITATION: Sweet image from the INTERNET
