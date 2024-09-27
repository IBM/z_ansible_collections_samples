/* REXX */
/******************************************************************************
* © Copyright IBM Corporation 2024
******************************************************************************/

parse arg 'name=' name

name = strip(name,'L')
say 'Hello ' || name || '.'

address syscall 'getcwd cwd'
say 'This Rexx script was run inside directory:' cwd

return 0
