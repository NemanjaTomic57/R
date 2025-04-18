#---------------------------------------------------------
# File:   mit18_05_s22_studio3-test.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 3 test script

# Before running: clean your environment and source('mit18_05_s22_studio3.r')

#-----------------------

studio3_problem_1a(1, 1000)
studio3_problem_1b(1, 1000)

#-----------------------
studio3_problem_2a(1, 1000)

studio3_problem_2b(1, 4000, 4, 0.05)
studio3_problem_2b(1, 4000, 8, 0.05)
studio3_problem_2b(1, 4000, 16, 0.05)
studio3_problem_2b(1, 4000, 64, 0.05)

cat("Wow! We just illustrated the Central Limit Theorem\n")
