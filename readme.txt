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

Currently just a library for parsing table/field definitions pumped out by
OpenEdge's data dictionary. It represents the tables and fields as maps for
fast access/comparison times.

Currently Working:
    - See common field names between two tables
    - Search all tables for a field name
    - Get info on a field in a table - its flags, format, etc.
    - Dump all tables to cStructs in order to use ctags/ vim's autocomplete (working,
        but only for c-conformant names; if you use dashes in your field
        names/ table names, ctags won't be able to parse it correctly.

In Progress:
    - Access over TCP
    - Graphical utility for Windows (IP)

Planned:
    - Weak intersections -- e.g., "ship-from" in one table weakly matches
                         -- "ship-to" in another
    - Database mapping - create a map of database intersections

USE:
    - Export a "simple" tables file from OpenEdge's DataDictionary, and name
      it "progressDB.txt".
    - To use the main utility, run the "haskellParsingSmall" executable.
    - To write the databse to c structs for autocomplete, run the "CStructDumper"
      executable with the "progressDB.txt" file in its directory.
