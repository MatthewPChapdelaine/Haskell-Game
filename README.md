# Haskell-Game
A Game written in Haskell by Grok.

Mathias Mindblade: Chaos RPG — a complete, self-contained text-based RPG written in pure Haskell (no external dependencies beyond base libraries).
File created:  (432 lines)

Features Implemented

Starring Mathias Mindblade — You play as the Chaos Mage himself. All flavor text, mechanics, and the story are built around his identity, ambition, and magickal theme (sigils, chaos points, the Void Codex, etc.).
Player Navigation System — 5 connected locations in a small open graph:
Arcane Sanctum (start)
Whispering Woods
Infernal Forge
Shadow Labyrinth
Void Throne (final area — locked behind the Key)

Player HUD — Clean bordered status display shown after every action. Shows location, health, Chaos Points, inventory, and any active enemy with HP.
Player Inventory — take / get items from the world, use / drink potions to heal, inventory / inv / i to list what you carry. Sword automatically gives combat bonus.
Point System — Chaos Points earned for:
First-time exploration (+15)
Repeat visits (+4)
Taking items (+6)
Defeating enemies (+30 normal / +120 for the final boss)

Winning State — Defeat the Void Sovereign at the Void Throne. Triggers a special victory screen and ends the game.
Losing State — Health reaches 0 during combat. Triggers a death screen.
Combat — Turn-based. attack / fight does one round (you strike first with scaling damage from Sword + Chaos Points, then enemy counters). flee lets you escape with a small point penalty.
Other polish — look, stats, help, quit. Special navigation rule (you need the Key from the Shadow Labyrinth to reach the final throne). Progressive difficulty and meaningful choices.

How to Run

Install Haskell (GHC) if you don't have it — easiest via ghcup.

Compile & play:

ghc Mathias_Mindblade_Chaos_RPG.hs -o mindblade
./mindblade

Or run directly without compiling:

runhaskell Mathias_Mindblade_Chaos_RPG.hs

Quick Start Tips (In-Game)

Type help anytime.
Explore everywhere and find the Key before heading to the Void Throne.
Use the Sword for big damage bonuses.
Potions are powerful — save or use wisely.
The final boss hits hard. Stack Chaos Points and the Sword before facing him.

This is a fully playable, self-contained RPG that hits every requirement you asked for. The code is clean, well-commented, and easy to extend (add more locations, items, spells, or even a simple save system later).
Enjoy your ascent to the Void Throne, Chaos Mage. The Codex awaits. 🌀
Would you like me to add any expansions (more enemies, spells that cost Chaos Points, multiple endings, a simple map command, etc.)?

