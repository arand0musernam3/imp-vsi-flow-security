# Todays dissucion
    We have decided to not implement scoping for anything besides functions.
    We had a plan as for how to do it, but we thought it would be too much work, non-IFC focused as it is "simply" compiler decisition. 
    We belive that it is still sound as current solution. We initilize each fresh variable in a function to be the same level as the caller PC level, to avoid leaks or crashes inside the function (caused by NSU). Since, none of those delcartion, are visiable outside the scope of the function, they cannot implicitly influrence the later variables and the flow of the program. The only way to perisist the value of the that (the fresh variable), is to return it and assign it to another variable on the caller side. 


# Tomorrows headace
    We need to clean up our test. 
    Make it clear what the different types of tests are. This needs to be very intuiative.
    Slightly begin how the proofs are structured. A general idea
