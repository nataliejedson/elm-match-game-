## Description 

A connect-the-dots type matching game written in Elm. 

## Current issues 

- there is something I'm not quite understanding about how to get actual objects from a JSON decoder--this is preventing me from being able to construct a model 
- the initial design of the app depends on the game item pks being sequential from 1-6—since that clearly won't be the case with game item data, how do we account for that?
- when the game is embedded in a page, the mouse subscription will no longer work. 
- the program is weak because of its inability to deal with maybe types
- I need to learn about AJAX in order to grab JSON and pull it through a port. 