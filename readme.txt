===========================================================================                         
                          _______ _______ _______ 
                          |\     /|\     /|\     /|
                          | +---+ | +---+ | +---+ |
                          | |   | | |   | | |   | |
                          | |B  | | |B  | | |L  | |
                          | +---+ | +---+ | +---+ |
                          |/_____\|/_____\|/_____\|
                                                   
                       Short for: Easy To Remember Name 
                  (Thanks to patorjk.com for the ascii art)
===========================================================================

A group of three tools for Progress (Openedge ABL) Programmers

DBParser.haskellParsingSmall:
  A Data Dictionary command line app for when you know what you want.
    - See common field names between two tables (intersect)
    - Search all tables for a field name (find)
    - Get info on a field in a table - its flags, format, etc. (info)
    - Weakly search a table - "Weakfind <table> 'num'" will find all 
      fields with num in their name

DBParser.cStructDumper:
    - Dump all from a DB file to cStructs in order to use ctags/ vim's
      autocomplete (working, but only for c-conformant names; if you 
      use dashes in your field names/ table names, ctags won't be able
      to parse it correctly).

IncludeFinder:
    - Print out a tree of every file that a file you specify includes, 
      written to disk as "file.extension.includes"

#######
Planned
#######

DBParser.haskellParsingSmall:
    - Weak intersections -- e.g., "ship-from" in one table weakly matches
                         -- "ship-to" in another
    - Database mapping - create a map of database intersections

DBParser.cStructDumper
    - None

IncludeFinder:
    - creating a forest of includes that can be queried to find out what files 
      include a file X

####
Use
####

DBParser utilities:
    - Export a "simple" tables file from OpenEdge's DataDictionary, and name
      it "progressDB.txt".
    haskellParsingSmall:
        - run the "haskellParsingSmall" executable.
    cStructDumper:
        - To write the databse to c structs for autocomplete, run the "CStructDumper"
          executable with the "progressDB.txt" file in its directory.

IncludeFinder:
    - Run the exe with the filename as an argument, or type in the file name
      when the program prompts for it.
