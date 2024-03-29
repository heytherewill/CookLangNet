﻿module IntegrationTests

(* These test public API of the parser against actual recipes to verify it works in the real world. *)

open CookLangNet
open Xunit
open FsUnit.Xunit
open FsUnit.CustomMatchers
open FsCheck.Xunit
open System.Collections.Generic

let tofuTikkaMasala =
    @"
>> source: https://rainbowplantlife.com/tofu-tikka-masala/

Prepare the @tofu. Use a #tofu press{}. Or, wrap the tofu in a clean dish towel or several layers of paper towels, weigh it down with a heavy cookbook or a large plate weighed down by a few cans of beans. Press the tofu for ~pressing tofu{30%minutes}. Tear the tofu into chunks with your hands and transfer the tofu chunks to a #large bowl{}. -- The tofu chunks shouldn’t be too large or too small (see photos for reference), but it’s totally fine if they’re not all the same size.

Make the tikka spice blend. Heat a small or medium #frying pan{} over medium heat. Once warm, add the whole spices (@cumin and @coriander seeds{}, @cloves, @peppercorns, @cardamom seeds{}, @cinnamon sticks{}, and @fenugreek leaves{}).

Toast, shaking the pan frequently, until they are very fragrant and toasty, and the lighter seeds have browned, about ~{3%minutes}. Transfer the seeds out of the pan and allow to cool.

Once cool, add to a #spice grinder{} or a #mortar & pestle and crush until finely ground. Pour into a bowl, add the ground spices (@Indian red chile powder{}, @turmeric, @paprika, @nutmeg, and @ginger{}), and toss all the spices to combine.
Make the Tikka marinade. In a medium bowl, mix together the spice blend with the @yogurt, @garlic, @ginger, @lemon juice{}, @oil, and @salt. Pour the marinade over the tofu chunks and use your hands to very gently coat all the crevices of the tofu, taking care to not mash it.
Cover and refrigerate for ~{2%hours}, or up to ~{8%hours}.
When ready to bake the tofu, arrange an #oven rack 6 inches from the heat source (usually, the second rack). Add a #12-inch cast iron skillet{} to the rack and preheat the oven to 500ºF/260ºC.
Once the skillet is smoking hot, transfer it to the stove or a pot holder. Turn the #broiler to high.
Add enough @oil to lightly coat the bottom of the pan. Add the marinated tofu to the pan, leaving excess marinade behind. It will be pretty tightly packed into the pan.
Return the pan to the second oven rack and broil until the tofu is cooked through and charred in some spots on top, about ~{11%minutes}. -- After the 5-minute mark, I recommend checking every 1-2 minutes to prevent burning since every oven is different.
MAKE THE MASALA. Heat the 3 tablespoons of oil in a deep #12-inch sauté pan{} over medium-high heat. Once hot, add the @cumin seeds{} and cook for 1 minute, swirling the pan frequently. Add the @dried chilies{} and cook for another ~{30%seconds}, swirling frequently to prevent burning.
Add the @onions with a couple pinches of @salt and cook until golden brown, about ~{6%minutes} (if the onions start browning too quickly, lower the heat to medium).
Add in a few splashes of water to stop the onions from browning too much, then add the @garlic, @ginger, diced @serrano pepper{}, and @turmeric, and cook for ~{1%minute}, tossing frequently. Add the @red chili powder{}, @coriander, and @tomato paste{}. Stir frequently for 30 to 60 seconds.
Add the @tomatoes and their juices, and cook until broken down and softened, ~{2.5%minutes}.
Add the @cilantro stems{} (save the leaves for the garnish), 3 tablespoons of water, and 1 ½ teaspoons @kosher salt{}, and stir. Cover the pan and bring to a simmer.
Simmer the masala for 15 minutes, opening the pot to stir occasionally, until the liquid has mostly evaporated. Scoop out the dried red chilies and discard.
Add in the @coconut milk{}, @garam masala{}, and @vegan butter{}. Crush the @fenugreek leaves{} in your hand to release the aroma and add to the masala. Increase the heat slightly and stir until everything is combined and the butter is melted.
If you prefer a smoother texture, feel free to run an #immersion blender{} through the masala.
Add the baked tofu, the thinly sliced serrano pepper (if using for spicy spicy!), and @lemon juice{}. Simmer for 2 minutes and coat the tofu in the sauce. Taste, and if it’s a bit too acidic, add a tiny bit of @sugar{0.5%teaspoon}.
Garnish the masala with the @cilantro leaves{} and season to taste with salt. Serve tofu tikka masala over @rice and/or with @Indian flatbread{}.
"

module ParseStringTests =

    let tikkaMasalaRecipe =
        { Metadata = dict [ ("source", "https://rainbowplantlife.com/tofu-tikka-masala/") ]
          Steps =
            [ { Directions =
                  "Prepare the tofu. Use a tofu press. Or, wrap the tofu in a clean dish towel or several layers of paper towels, weigh it down with a heavy cookbook or a large plate weighed down by a few cans of beans. Press the tofu for 30 minutes. Tear the tofu into chunks with your hands and transfer the tofu chunks to a large bowl."
                Ingredients = [ { Name = "tofu"; Amount = None } ]
                Cookware =
                  [ { Quantity = Numeric 1
                      Name = "tofu press" }
                    { Name = "large bowl"
                      Quantity = Numeric 1 } ]
                Timers =
                  [ { Name = "pressing tofu"
                      Duration = (float 30)
                      Unit = "minutes" } ] }
              { Directions =
                  "Make the tikka spice blend. Heat a small or medium frying pan over medium heat. Once warm, add the whole spices (cumin and coriander seeds, cloves, peppercorns, cardamom seeds, cinnamon sticks, and fenugreek leaves)."
                Ingredients =
                  [ { Name = "cumin"; Amount = None }
                    { Name = "coriander seeds"
                      Amount = None }
                    { Name = "cloves"; Amount = None }
                    { Name = "peppercorns"; Amount = None }
                    { Name = "cardamom seeds"
                      Amount = None }
                    { Name = "cinnamon sticks"
                      Amount = None }
                    { Name = "fenugreek leaves"
                      Amount = None } ]
                Cookware =
                  [ { Quantity = Numeric 1
                      Name = "frying pan" } ]
                Timers = [] }
              { Directions =
                  "Toast, shaking the pan frequently, until they are very fragrant and toasty, and the lighter seeds have browned, about 3 minutes. Transfer the seeds out of the pan and allow to cool."
                Ingredients = []
                Cookware = []
                Timers =
                  [ { Name = ""
                      Duration = 3.0
                      Unit = "minutes" } ] }
              { Directions =
                  "Once cool, add to a spice grinder or a mortar & pestle and crush until finely ground. Pour into a bowl, add the ground spices (Indian red chile powder, turmeric, paprika, nutmeg, and ginger), and toss all the spices to combine."
                Ingredients =
                  [ { Name = "Indian red chile powder"
                      Amount = None }
                    { Name = "turmeric"; Amount = None }
                    { Name = "paprika"; Amount = None }
                    { Name = "nutmeg"; Amount = None }
                    { Name = "ginger"; Amount = None } ]
                Cookware =
                  [ { Quantity = Numeric 1
                      Name = "spice grinder" }
                    { Name = "mortar"
                      Quantity = Numeric 1 } ]
                Timers = [] }
              { Directions =
                  "Make the Tikka marinade. In a medium bowl, mix together the spice blend with the yogurt, garlic, ginger, lemon juice, oil, and salt. Pour the marinade over the tofu chunks and use your hands to very gently coat all the crevices of the tofu, taking care to not mash it."
                Ingredients =
                  [ { Name = "yogurt"; Amount = None }
                    { Name = "garlic"; Amount = None }
                    { Name = "ginger"; Amount = None }
                    { Name = "lemon juice"; Amount = None }
                    { Name = "oil"; Amount = None }
                    { Name = "salt"; Amount = None } ]
                Cookware = []
                Timers = [] }
              { Directions = "Cover and refrigerate for 2 hours, or up to 8 hours."
                Ingredients = []
                Cookware = []
                Timers =
                  [ { Name = ""
                      Duration = float 2
                      Unit = "hours" }
                    { Name = ""
                      Duration = float 8
                      Unit = "hours" } ] }
              { Directions =
                  "When ready to bake the tofu, arrange an oven rack 6 inches from the heat source (usually, the second rack). Add a 12-inch cast iron skillet to the rack and preheat the oven to 500ºF/260ºC."
                Ingredients = []
                Cookware =
                  [ { Quantity = Numeric 1; Name = "oven" }
                    { Name = "12-inch cast iron skillet"
                      Quantity = Numeric 1 } ]
                Timers = [] }
              { Directions =
                  "Once the skillet is smoking hot, transfer it to the stove or a pot holder. Turn the broiler to high."
                Ingredients = []
                Cookware =
                  [ { Quantity = Numeric 1
                      Name = "broiler" } ]
                Timers = [] }
              { Directions =
                  "Add enough oil to lightly coat the bottom of the pan. Add the marinated tofu to the pan, leaving excess marinade behind. It will be pretty tightly packed into the pan."
                Ingredients = [ { Name = "oil"; Amount = None } ]
                Cookware = []
                Timers = [] }
              { Directions =
                  "Return the pan to the second oven rack and broil until the tofu is cooked through and charred in some spots on top, about 11 minutes."
                Ingredients = []
                Cookware = []
                Timers =
                  [ { Name = ""
                      Duration = float 11
                      Unit = "minutes" } ] }
              { Directions =
                  "MAKE THE MASALA. Heat the 3 tablespoons of oil in a deep 12-inch sauté pan over medium-high heat. Once hot, add the cumin seeds and cook for 1 minute, swirling the pan frequently. Add the dried chilies and cook for another 30 seconds, swirling frequently to prevent burning."
                Ingredients =
                  [ { Name = "cumin seeds"; Amount = None }
                    { Name = "dried chilies"
                      Amount = None } ]
                Cookware =
                  [ { Quantity = Numeric 1
                      Name = "12-inch sauté pan" } ]
                Timers =
                  [ { Name = ""
                      Duration = float 30
                      Unit = "seconds" } ] }
              { Directions =
                  "Add the onions with a couple pinches of salt and cook until golden brown, about 6 minutes (if the onions start browning too quickly, lower the heat to medium)."
                Ingredients = [ { Name = "onions"; Amount = None }; { Name = "salt"; Amount = None } ]
                Cookware = []
                Timers =
                  [ { Name = ""
                      Duration = float 6
                      Unit = "minutes" } ] }
              { Directions =
                  "Add in a few splashes of water to stop the onions from browning too much, then add the garlic, ginger, diced serrano pepper, and turmeric, and cook for 1 minute, tossing frequently. Add the red chili powder, coriander, and tomato paste. Stir frequently for 30 to 60 seconds."
                Ingredients =
                  [ { Name = "garlic"; Amount = None }
                    { Name = "ginger"; Amount = None }
                    { Name = "serrano pepper"
                      Amount = None }
                    { Name = "turmeric"; Amount = None }
                    { Name = "red chili powder"
                      Amount = None }
                    { Name = "coriander"; Amount = None }
                    { Name = "tomato paste"; Amount = None } ]
                Cookware = []
                Timers =
                  [ { Name = ""
                      Duration = (float 1)
                      Unit = "minute" } ] }
              { Directions = "Add the tomatoes and their juices, and cook until broken down and softened, 2.5 minutes."
                Ingredients = [ { Name = "tomatoes"; Amount = None } ]
                Cookware = []
                Timers =
                  [ { Name = ""
                      Duration = float 2.5f
                      Unit = "minutes" } ] }
              { Directions =
                  "Add the cilantro stems (save the leaves for the garnish), 3 tablespoons of water, and 1 ½ teaspoons kosher salt, and stir. Cover the pan and bring to a simmer."
                Ingredients =
                  [ { Name = "cilantro stems"
                      Amount = None }
                    { Name = "kosher salt"; Amount = None } ]
                Cookware = []
                Timers = [] }
              { Directions =
                  "Simmer the masala for 15 minutes, opening the pot to stir occasionally, until the liquid has mostly evaporated. Scoop out the dried red chilies and discard."
                Ingredients = []
                Cookware = []
                Timers = [] }
              { Directions =
                  "Add in the coconut milk, garam masala, and vegan butter. Crush the fenugreek leaves in your hand to release the aroma and add to the masala. Increase the heat slightly and stir until everything is combined and the butter is melted."
                Ingredients =
                  [ { Name = "coconut milk"; Amount = None }
                    { Name = "garam masala"; Amount = None }
                    { Name = "vegan butter"; Amount = None }
                    { Name = "fenugreek leaves"
                      Amount = None } ]
                Cookware = []
                Timers = [] }
              { Directions =
                  "If you prefer a smoother texture, feel free to run an immersion blender through the masala."
                Ingredients = []
                Cookware =
                  [ { Quantity = Numeric 1
                      Name = "immersion blender" } ]
                Timers = [] }
              { Directions =
                  "Add the baked tofu, the thinly sliced serrano pepper (if using for spicy spicy!), and lemon juice. Simmer for 2 minutes and coat the tofu in the sauce. Taste, and if it’s a bit too acidic, add a tiny bit of sugar."
                Ingredients =
                  [ { Name = "lemon juice"; Amount = None }
                    { Name = "sugar"
                      Amount =
                        Some
                            { Quantity = Numeric 0.5
                              Unit = Some "teaspoon" } } ]
                Cookware = []
                Timers = [] }
              { Directions =
                  "Garnish the masala with the cilantro leaves and season to taste with salt. Serve tofu tikka masala over rice and/or with Indian flatbread."
                Ingredients =
                  [ { Name = "cilantro leaves"
                      Amount = None }
                    { Name = "rice"; Amount = None }
                    { Name = "Indian flatbread"
                      Amount = None } ]
                Cookware = []
                Timers = [] } ] }

    [<Fact>]
    let ``Parsing a full recipe via the public API works`` () =
        match CookLangParser.parse tofuTikkaMasala with
        | Success parsedRecipe -> parsedRecipe |> shouldBeTheSameRecipeAs tikkaMasalaRecipe
        | Failure err -> failwith err
