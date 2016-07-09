This is me learning F# by implementing a parser for some yet unspecified language (lua). 

The goal is to get to know F# really good and not to produce a fancy, pretty or production ready parser...

I started out trying to implement a parser for regular expressions by handcoding everything and only using 1 look forward. 
Although this worked it got rather complex and was not very smart implemented. After some time i discovered FParsec http://www.quanttec.com/fparsec/. 
This gave new ideas to how to implement my lua parser. To acomplis my goal of learning F# I will NOT use the FParsec library, although it looks really cool! 
I will by looking at the FParsec documentation make my own subset implementation of FParsec and use that for my lua parser. 
I will keep the the CharStream as simple as possible and not use RegEx from the .NET library.

