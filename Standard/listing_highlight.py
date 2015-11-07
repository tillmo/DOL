#!/usr/bin/env python3

import sys

inputDiff = sys.argv[1]
outputDiff = sys.argv[2]

#with open("OMG_OntoIOp_current-diff2e94c79.tex", "r") as readDiff:
with open(inputDiff, "r") as readDiff:
      
#      with open("OMG_diff.tex", "w") as writeDiff:
      with open(outputDiff, "w") as writeDiff:
            begin_listing = r"\begin{lstlisting}"
            end_listing = "\end{lstlisting}"

            begin_delete = "\DIFdelbegin"
            end_delete = "\DIFdelend"    

            begin_add = "\DIFaddbegin"
            end_add = "\DIFaddend"

            begin_listing_flag = "false"
            end_listing_flag = "false"
            
#            begin_delete_count = 0
#           end_delete_count = 0

#            begin_add_count = 0
#            end_add_count = 0

            exist_escape_1 = "escapeinside={()}"
            exist_escape_2 = "escapeinside={<>}"
                            
            escape_strings = '''\lstset{escapeinside = {*@}{@*}}
'''            

            for line in readDiff:
                  if begin_listing in line:                                                                                   # search \begin{lstlisting}
                        begin_listing_flag = "true"                                                                           # note the start of lstlisting
                        end_listing_flag = "false"                                                                            # \end{lstlisting} not reached   
                        if exist_escape_1 in line:
                              first_escape = " ( "
                              second_escape = " ) "
                        elif exist_escape_2 in line:
                              first_escape = " < "
                              second_escape = " > "
                        else:
                              first_escape = " *@ "
                              second_escape = " @* "
                              line = line.replace(line, escape_strings + line)                                                # set escape string to escape to latex 
                  if end_listing in line:                                                                                     # search \end{lstlisting}
                        end_listing_flag = "true"                                                                             # end of \begin{lstlisting} is reached      
                        begin_listing_flag = "false"                                                                          # note the finish of lstlisting
                  if begin_listing_flag == "true" and end_listing_flag == "false" and begin_delete in line:                   # search \DIFdelbegin in the line 
                        line = line.replace(begin_delete, first_escape + begin_delete)                                        # add escape string to escape to latex to execute \DIFdel
#                        begin_delete_count = begin_delete_count + 1         
                  if begin_listing_flag == "true" and end_listing_flag == "false" and end_delete in line:                     # search \DIFdelend in the line      
                        line = line.replace(end_delete, end_delete + second_escape)                                           # add escape string to return to the lstlisting environment
#                       end_delete_count = end_delete_count + 1      
                  if begin_listing_flag == "true" and end_listing_flag == "false" and begin_add in line:                      # search \DIFaddbegin in the line                   
                        line = line.replace(begin_add, first_escape + begin_add)                                              # add escape string to escape to latex to execute \DIFadd
#                        begin_add_count = begin_add_count + 1         
                  if begin_listing_flag == "true" and end_listing_flag == "false" and end_add in line:                        # search \DIFaddend in the line
                        line = line.replace(end_add, end_add + second_escape)                                                 # add escape string to return to the lstlisting environment
#                     end_add_count = end_add_count + 1      

                  writeDiff.write(line)

#print begin_delete_count
#print end_delete_count
#print begin_add_count
#print end_add_count


  
readDiff.close()
writeDiff.close()




#\lstset{
#    escapeinside={(*@}{@*)},          
#}



      
