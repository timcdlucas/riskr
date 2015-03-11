riskr
======


An R package for calculating Risk battle odds.

![Rimmer from Red Dwarf](hqdefault.jpg)


There are quite a few Risk battle odds on the internet. But I didn't find one that calculates odds for a whole string of battles which is what we often want to know.
For example, you have 20 units in North Africa. What are the odds you can take Brazil (2 units), Venezuala (3 units) and Central America (10 units) and thus deny your opponent their N. America bonus units?

    1> fight(10, c(2,3,10))
    Attacker wins 8%


To do: 
- Write a summary function that tells you how many units the attacker has left or how many countries they took if all their units died.
- Plot histograms.


